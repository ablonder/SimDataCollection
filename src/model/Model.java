package model;
/*
 * Superclass for running and gathering data from models
 * 
 * @author Aviva Blonder
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import ec.util.MersenneTwisterFast;
import sim.engine.*;
import sim.field.network.Edge;
import sim.field.network.Network;
import sim.util.distribution.Beta;
import sim.util.distribution.Distributions;

public abstract class Model extends SimState  {

	// array to hold parameter values
	public String[] params;
	// initialize empty array of parameters to test
	public ArrayList<Integer> testparams = new ArrayList<Integer>();
	// initialize empty nested array of test parameter values
	public ArrayList<ArrayList<String>> testvals = new ArrayList<ArrayList<String>>();
	// initialize empty array of parameters to randomize
	public ArrayList<Integer> randparams = new ArrayList<Integer>();
	// initialize empty array of distributions to draw randomly from
	public ArrayList<String> randdists = new ArrayList<String>();
	// file writer for the end of run results
	public BufferedWriter endwriter;
	// file writer for timecourse results taken at the indicated interval
	public BufferedWriter timewriter;
	// file writer for individual agent results (only created if results are provided to be taken)
	public BufferedWriter agentwriter;
	// list of file writers for edgelists (only created if networks are provided to get edgelists from)
	public BufferedWriter[] netwriters;
	// file writer for list results at the model level
	public BufferedWriter listwriter;
	// file writer for list results at the agent level
	public BufferedWriter agentlistwriter;
	// the separator for writing results to file
	public char sep = ',';
	// key parameters used in every model (as class variables for access if the user wants them)
	public static String[] keyparams = {"seed", "sep", "steps", "iters", "reps", "fname", "testint", "teststart",
			"gui", "agentint", "netint", "listint"};
	public int seed = 0;
	public int steps;
	public int iters = 1;
	public int reps = 1;
	public String fname = "";
	public int testint;
	public int teststart = 0;
	public boolean gui = false;
	public int agentint = 0;
	public int netint = 0;
	public int listint = 0;
	// list of parameter names - to be made in the child class
	public String[] paramnames;
	// indicates whether to automatically use the subclass fields as parameters - default to true
	public boolean autoparams = true;
	// list of names of categories of data to gather - to be made in the child class
	public String[] resnames;
	// indicates whether to automatically use the remaining subclass fields as results categories
	// defaults to true, but is only relevant when reading from file
	public boolean autores = true;
	// list of results to be gathered from each agent individually
	public String[] agentres = new String[0];
	// list of agents
	public Object[] agents;
	// list of networks to be gathered from the model
	public String[] nets = new String[0];
	// list of list type results to be gathered from the model
	public String[] lists = new String[0];
	// and from the agents
	public String[] agentlists = new String[0];
	// list of results names added from file for splitting
	public String[] fileres;
	// the subclass for use in accessing its fields
	public Class subclass;
	// the agent class for use in accessing its fields
	public Class agentclass;

	/*
	 * constructor when no file or arguments are given that just creates a template for input
	 */
	public Model() {
		super(0);
		makeInputFile();
	}

	/*
	 * constructor that handles file input
	 */
	public Model(String fname) {
		// I have to do this first, so I will, and I'll just have to reseed from file later
		super(0);
		String[] args = readFile(fname);
		//and call run on args
		run(args);
	}

	/*
	 * constructor that handles command line input
	 * TODO - get this to work with keyparams or drop entirely
	 */
	public Model(String[] args) {
		// runs the superclass constructor
		super(Integer.parseInt(args[0]));
		// creates the list of parameter names from the subclass
		setNames();
		// also get the subclass
		setClasses();
		// if all the fields should automatically be added as parameters, add those
		if(this.autoparams) {
			// initialize an array list to hold the parameter names to make this easier
			ArrayList<String> tempparams = new ArrayList<String>();
			// then get all the fields
			Field[] fields = subclass.getDeclaredFields();
			for(int f = 0; f < fields.length; f++) {
				// make sure each field is a type it can handle
				Class c = fields[f].getType();
				if(c == String.class || c == Integer.TYPE || c == Double.TYPE || c == Character.TYPE || c == Boolean.TYPE)
					tempparams.add(fields[f].getName());
			}
			// now add it back to the list of params (which takes a bit of doing)
			ArrayList<String> pars = new ArrayList<String>(Arrays.asList(paramnames));
			pars.addAll(tempparams);
			paramnames = pars.toArray(new String[pars.size()]);
		}
		// now it can all run
		run(args);
	}
	
	public String[] readFile(String fname) {
		// run set names to initialize everything
		setNames();
		// I'm going to start with an array list of parameters because it needs to grow
		ArrayList<String> tempparams = new ArrayList<String>();
		// and an array list of parameter values
		ArrayList<String> tempvals = new ArrayList<String>();
		// and an array list of results variables
		ArrayList<String> tempres = new ArrayList<String>();
		// and an array list of list-type results
		ArrayList<String> templistres = new ArrayList<String>();
		try {
			// now to open the file and start reading
			BufferedReader file = new BufferedReader(new FileReader(fname));
			// loop through the file
			String line;
			while((line = file.readLine()) != null) {
				// % is used as a comment character, so skip any line that starts with it
				if(line.length() < 1 || line.charAt(0) == '%') continue;
				// and otherwise only look at what comes before a %
				String[] readline = line.split("%");
				// then split each line at the equals sign
				String[] splitline = readline[0].split("=");
				// first check for lines without anything after the equals, or no equals at all
				if(splitline.length < 2 || splitline[1].trim().length() < 1) {
					// if there's actually something there and we're automatically gathering results, do that
					if(this.autores && splitline[0].length() > 0) {
						String r = splitline[0].trim();
						// check if it's a list
						try {
							Field f = this.subclass.getField(r);
							Class t = f.getType();
							if(Collection.class.isAssignableFrom(t) || Array.class.isAssignableFrom(t)) {
								templistres.add(r);
							} else {
								tempres.add(r);
							}
						}catch(NoSuchFieldException e) {
							tempres.add(r);
						}
					}
				} else if(this.autores && splitline[0].trim().equals("*agentInfo") && splitline[1].length() > 1) {
					// otherwise, try to catch agentInfo (which should be a key parameter)
					// split the parameters to get the data gathered from each agent
					ArrayList<String> newagentres = new ArrayList<String>(Arrays.asList(splitline[1].trim().split(" ")));
					// also get the list of list-type agent parameters
					ArrayList<String> alistres = new ArrayList<String>(Arrays.asList(this.agentlists));
					// grab the existing list of agent parameters
					ArrayList<String> oldagentres = new ArrayList<String>(Arrays.asList(this.agentres));
					// loop through the new list of agent parameters to add to the list (and check for lists)
					for(String r : newagentres) {
						// check if the parameter is a list of some type
						try {
							Field f = this.agentclass.getField(r);
							Class t = f.getType();
							if(Collection.class.isAssignableFrom(t) || Array.class.isAssignableFrom(t)) {
								alistres.add(r);
							} else {
								oldagentres.add(r);
							}
						}catch(NoSuchFieldException e) {
							oldagentres.add(r);
						}
					}
					// store the resulting complete lists as arrays
					this.agentres = oldagentres.toArray(new String[oldagentres.size()]);
					this.agentlists = alistres.toArray(new String[alistres.size()]);
				} else if(this.autores && splitline[0].trim().equals("*edgeList") && splitline[1].length() > 1) {
					// also catch the edgeList (which should also be a key parameter)
					// split the following parameters to get a list of network names to create edgelists of
					this.nets = splitline[1].trim().split(" ");
				}else if(splitline[1].length() > 1) {
					// otherwise, if there's something after the equals sign, add it to params
					// check for key parameters marked by a star
					if(line.charAt(0) == '*' && Arrays.asList(keyparams).contains(splitline[0].trim().substring(1))) {
						// modify the parameter's value
						setParamVal(Model.class, splitline[0].trim().substring(1), splitline[1].trim());
					} else {
						// add the first part to the list of parameter names (via tempparams)
						tempparams.add(splitline[0].trim());
						// and the second part to the list of parameter values (via tempvals)
						tempvals.add(splitline[1].trim());
					}
				}
			}
			// now these lists can become/be added to the official lists
			this.paramnames = tempparams.toArray(new String[tempparams.size()]);
			String[] args = tempvals.toArray(new String[tempvals.size()]);
			// the results will take a little more doing because they have to be concatenated
			// first store the added results just in case this is going to be used to split files
			this.fileres = tempres.toArray(new String[tempres.size()]);
			// then concatenate the added parameters to the initialized list
			ArrayList<String> res = new ArrayList<String>(Arrays.asList(resnames));
			res.addAll(tempres);
			resnames = res.toArray(new String[res.size()]);
			// and also save the list results
			this.lists = templistres.toArray(new String[templistres.size()]);
			// now I can set the seed
			this.setSeed(this.seed);
			// and return args
			return args;
		} catch (FileNotFoundException e) {
			System.out.println("Input file not found!");
			System.exit(0);
		} catch (IOException e) {
			System.out.println("Problem reading input file!");
			System.exit(0);
		}
		// this should never be triggered
		return null;
	}

	/*
	 * checks all the arguments and manages the runs
	 */
	public void run(String[] args) {
		// make sure there are enough parameters
		if(args.length < paramnames.length) {
			System.out.println("Not enough arguments!");
			System.exit(0);
		}
		// initialize the list of params
		this.params = new String[paramnames.length];
		// run through args to initialize the base params and determine which parameters to test
		for(int i = 0; i < paramnames.length; i++) {
			parse(i, args[i]);
		}
		// if gui is true, just set the parameters and let the gui run
		if(gui) {
			setParams(params);
		} else {
			// otherwise create an output file and sweep
			// start by making sure all the necessary key parameters are there
			if(steps == 0 || reps == 0 || testint == 0) {
				System.out.println("Missing key parameters for running without GUI (steps, reps, or testint)!");
				System.exit(0);
			}
			try {
				// if there are whole model results, create files for those (one just at the end and one timecourse)
				if(this.resnames.length > 0) {
					// make files to write the results to
					this.endwriter = new BufferedWriter(new FileWriter(this.fname+"endresults.txt"));
					this.timewriter = new BufferedWriter(new FileWriter(this.fname + "timeresults.txt"));
					// and write in a header
					makeHeader(this.endwriter, false, false, this.resnames);
					makeHeader(this.timewriter, true, false, this.resnames);
				}
				// if there are agent results, also create a file to hold those
				if(this.agentres.length > 0) {
					this.agentwriter = new BufferedWriter(new FileWriter(this.fname + "agentresults.txt"));
					makeHeader(this.agentwriter, true, true, this.agentres);
				}
				// if there are network results, also create a file for each of those
				if(this.nets.length > 0) {
					// initialize a list of files equal to the number of networks
					this.netwriters = new BufferedWriter[this.nets.length];
					for(int i = 0; i < this.nets.length; i++) {
						// then create each file
						this.netwriters[i] = new BufferedWriter(new FileWriter(this.fname + this.nets[i] + "edgelist.txt"));
						makeHeader(this.netwriters[i], true, false, new String[]{"from", "to", "info"});
					}
				}
				// and if there are list results, create files for those at the model level
				if(this.lists.length > 0) {
					this.listwriter = new BufferedWriter(new FileWriter(this.fname + "listresults.txt"));
					makeHeader(this.listwriter, true, false, new String[] {"List", "Values"});
				}
				// and at the agent level
				if(this.agentlists.length > 0) {
					this.agentlistwriter = new BufferedWriter(new FileWriter(this.fname + "agentlistresults.txt"));
					makeHeader(this.agentlistwriter, true, true, new String[] {"List", "Values"});
				}
			} catch(IOException e) {
				System.out.println("Something's wrong with your results files!");
				System.exit(0);
			}
			// draw random parameters from a separate random seed
			MersenneTwisterFast paramgen = new MersenneTwisterFast();
			paramgen.setSeed(this.seed);
			// draw the designated number of random iterations (at least 1), and then sweep parameters/test each
			for(int i = 0; i < Math.max(this.iters, 1); i++) {
				// randomly draw all the randparams
				for(int r = 0; r < this.randparams.size(); r++) {
					double val = parseRand(paramgen, this.randdists.get(r));
					// if that's NaN, something didn't work, and print out a message, but keep going
					if(Double.isNaN(val)) {
						System.out.println("Random parameter not formatted correctly.");
					}
					this.params[this.randparams.get(r)] = String.valueOf(val);
				}
				// then, if there are no test params, just test
				if(this.testparams.size() == 0) {
					// test the model for the given seed, number of steps, replications,
					// and the test interval
					test();
				} else {
					// otherwise sweep
					sweep(0);
				}
			}
			try {
				if(this.resnames.length > 0) {
					this.endwriter.close();
					this.timewriter.close();
				}
				if(this.agentres.length > 0) {
					this.agentwriter.close();
				}
				if(this.nets.length > 0) {
					for(int i = 0; i < this.nets.length; i++) {
						this.netwriters[i].close();
					}
				}
			} catch (IOException e) {
				System.out.println("Writer not closing...");
			}
		}
		System.out.println("done.");
	}

	/*
	 * parses inputed strings into lists of variables
	 */
	public void parse(int param, String val) {
		// first check to see if it's to be randomly drawn (if the parser doesn't return Not a Number)
		double draw = parseRand(this.random, val);
		if(!Double.isNaN(draw)) {
			// then add it to randparams
			this.randparams.add(param);
			// store the indicated distribution
			this.randdists.add(val);
			// and draw a value for it
			this.params[param] = Double.toString(draw);
		} else {
			// otherwise, check to see if its a test value (for now these are mutually exclusive)
			// split the value between spaces
			String[] vals = val.split(" ");
			// the first value is the starting value of that parameter
			this.params[param] = vals[0];
			// if there are more values provided store them for testing
			if(vals.length > 1) {
				// first store this parameter
				this.testparams.add(param);
				// then store the remaining values
				this.testvals.add((new ArrayList<String>(Arrays.asList(vals))));
			}
		}
	}
	
	/*
	 * parses and randomly draws such parameters (if the code is invalid, it returns NaN)
	 */
	public double parseRand(MersenneTwisterFast rand, String code) {
		// first remove all whitespace
		code = code.replaceAll(" ", "");
		// then grab the distribution (which should be a letter: N for normal, U for uniform, and C for choice)
		char dist = code.charAt(0);
		// make sure there are parentheses, so I'm not just taking random things and declaring them to be random draws
		if(code.length() < 4 || code.charAt(1) != '(' || code.charAt(code.length()-1) != ')') return Double.NaN;
		// then split parameters within the parentheses using commas
		String[] distparams = code.substring(2,code.length()-1).split(",");
		// now I'm going to assume the values inside are doubles, and if that fails, return NaN, to show  it failed
		try {
			switch(dist) {
			case 'N':
				// normal with mean and standard deviation
				return rand.nextGaussian()*Double.parseDouble(distparams[1]) + Double.parseDouble(distparams[0]);
			case 'U':
				// uniform with min and max
				return rand.nextDouble()*(Double.parseDouble(distparams[1])-Double.parseDouble(distparams[0])) + Double.parseDouble(distparams[0]);
			case 'C':
				// choice between a provided number of options (mostly used for a coin flip on binary variables) 
				return rand.nextInt(Integer.parseInt(distparams[0]));
			case 'G':
				// Gamma distribution, for things that are generally small (but above 0), with some larger values - with optional min
				if(distparams.length < 3) return(distSampler.drawGamma(rand, Double.parseDouble(distparams[0]), Double.parseDouble(distparams[1]), 0));
				return distSampler.drawGamma(rand, Double.parseDouble(distparams[0]), Double.parseDouble(distparams[1]), Double.parseDouble(distparams[2]));
			}
		}catch(NumberFormatException e) {
			// this and a few other things will result in returning NaN
		}
		return Double.NaN;
	}

	/*
	 * Writes the header for a results file
	 */
	public void makeHeader(BufferedWriter writer, boolean time, boolean agent, String[] res) {
		try {
			// start with the base parameters
			writer.write("% Base Parameters: ");
			// loop through each parameter and its base values
			for(int p = 0; p < this.params.length; p++) {
				writer.write(paramnames[p]);
				writer.write(" = ");
				writer.write(params[p]);
				writer.write(", ");
			}
			// new line
			writer.write("\n\n");
			// list random parameters
			writer.write("% Random Parameters: \n");
			// loop through each random parameter and its distribution
			for(int r = 0; r < this.randparams.size(); r++) {
				writer.write("%" + paramnames[this.randparams.get(r)]);
				writer.write(" = ");
				writer.write(this.params[this.randparams.get(r)]);
				writer.write("\n");
			}
			// Now for the test parameters
			writer.write("% Test Parameters:\n");
			// loop through each test parameter and its values
			for(int t = 0; t < this.testparams.size(); t++) {
				writer.write("%" + paramnames[this.testparams.get(t)]);
				writer.write(" = ");
				writer.write(this.testvals.get(t).toString());
				writer.write("\n");
			}
			// make the header for the table
			// start this next line with a percent sign to comment it out for analysis
			// and a separator to make space for the seed
			writer.write("% " + this.sep);
			// if this file will hold time results, add an extra separator to make space for the time
			if(time) {
				writer.write(this.sep);
			}
			// and if this file will hold agent results, add an extra separator to make space for the agent
			if(agent) {
				writer.write(this.sep);
			}
			// indicate the random parameters
			if(this.randparams.size() > 0) writer.write("Random Parameters");
			// then leave enough space for each parameter
			for(int r = 0; r < this.randparams.size(); r++) writer.write(this.sep);
			// and indicate the test parameters if there are any
			if(this.testparams.size() > 0) writer.write("Test Parameters");
			// leave enough space for each parameter
			for(int t = 0; t < this.testparams.size(); t++) writer.write(this.sep);
			// then indicate categories for the results
			writer.write("Results\n");
			// start with the seed
			writer.write("Seed" + this.sep);
			// if this file will hold time results, add an extra column for the timestep
			if(time) {
				writer.write("Timestep" + this.sep);
			}
			// loop through all the random parameters and add headers for each
			for(int r = 0; r < this.randparams.size(); r++) {
				writer.write(this.paramnames[this.randparams.get(r)] + this.sep);
			}
			// then loop through all the test parameters and add headers for each parameter
			for(int t = 0; t < this.testparams.size(); t++) {
				writer.write(this.paramnames[this.testparams.get(t)] + this.sep);
			}
			// if this file will hold agent data, add the headers for the categories of agent results
			if(agent) {
				writer.write("AgentID" + this.sep + "Agent" + this.sep);
			}
			// add the headers for all the results
			for(int r = 0; r < res.length; r++) {
				writer.write(res[r] + this.sep);
			}
			// and then do a line break
			writer.write("\n");
		} catch(IOException e) {
			System.out.println("Something went wrong while making the header...");
			System.exit(0);
		}
	}

	/*
	 * In charge of running the sweeps recursively
	 */
	public void sweep(int tdex) {
		// test each value of this parameter
		for(int v = 0; v < this.testvals.get(tdex).size(); v++) {
			// create a copy of params with this parameter value
			//ArrayList<String> paramcopy = new ArrayList<String>(params);
			this.params[this.testparams.get(tdex)] = this.testvals.get(tdex).get(v);
			// if this isn't the last parameter, test the values of the remaining parameters
			if(tdex < this.testparams.size()-1) {
				sweep(tdex+1);
			} else {
				// otherwise, run this simulation
				test();
			}
		}
	}

	/*
	 * Actually runs the simulation on the provided parameters
	 */
	public void test() {
		System.out.println(Arrays.asList(this.params).toString());
		// store the parameters for this run as a string for writing to file
		String p = "";
		// followed by all the random and test parameter values
		for(int r = 0; r < this.randparams.size(); r++) {
			p += this.params[this.randparams.get(r)] + this.sep;
		}
		for(int t = 0; t < this.testparams.size(); t++) {
			p += this.params[this.testparams.get(t)] + this.sep;
		}
		// run the same simulation for the designated number of replications
		for(int i = 0; i < reps; i++) {
			// set model parameters from args (needs to be done fresh each time or they can build)
			setParams(this.params.clone());
			// store the seed for this run
			int s = seed+i;
			// reseed with the seed parameter, plus the replication number
			random.setSeed(s);
			// start the simulation
			start();
			// run the simulation for the designated number of steps
			while(schedule.getSteps() < steps) {
				// if this is the right step according to the test interval, write the results for this step
				if(schedule.getSteps() >= teststart && schedule.getSteps()%testint == 0) {
					writeResults(s, p, false);
				}
				if (!schedule.step(this)) break;
			}
			// get the end results once it's all done
			writeResults(s, p, true);
			finish();
		}
	}

	
	/*
	 * Allows the subclass to alter the value of a param (without having to mess with params directly...)
	 */
	public void resetParam(String param, String val) {
		// get index of param from paramnames, and then reassign that value in params
		this.params[Arrays.asList(paramnames).indexOf(param)] = val;
		// TODO - catch if it's not a real param
	}
	
	/*
	 * creates the lists of parameter names (paramnames) and result categories (resnames),
	 * can also change whether to automatically get params and results
	 */
	public void setNames() {
		// defaults to an empty list of parameters that will be filled in by the subclass's fields
		this.paramnames = new String[0];
		// and an empty list of result categories (will only be filled in if reading from file)
		this.resnames = new String[0];
		// and an empty list of agent results
		this.agentres = new String[0];
	}
	
	/*
	 * The subclass has to set subclass to store its own type and agent to store the agent type
	 */
	public abstract void setClasses();

	/*
	 * takes an inputed list of strings and uses it to set parameter values
	 * the default is to use the parameter names as field names and assume they line up
	 */
	public void setParams(String[] params) {
		// get the subclass
		setClasses();
		// loop through all named results and initialize to zero
		for(int r = 0; r < resnames.length; r++) {
			setParamVal(subclass, resnames[r], "0");
		}
		// reinitialize the list of agents to an empty list
		this.agents = new Object[0];
		// loop through all named parameters
		for(int p = 0; p < paramnames.length; p++) {
			// if that parameter is an actual field, assign its value based on params (or do what the user changes it to do)
			setParamVal(subclass, paramnames[p], params[p]);
		}
	}

	/*
	 * Actually tries to set a given parameter to a given value (provided as a string)
	 * because I need to use this in a few places
	 */
	public void setParamVal(Class c, String pname, String pval) {
		try{
			Field f = c.getField(pname);
			Class t = f.getType();
			// if the provided value parses as instructions for drawing randomly, do that instead
			double draw = parseRand(this.random, pval);
			if(!Double.isNaN(draw)) {
				pval = Double.toString(draw);
				System.out.println(draw);
			}
			if(t == String.class) f.set(this, pval);
			else if(t == Integer.TYPE) f.setInt(this, (int) Double.parseDouble(pval));
			else if(t == Double.TYPE) f.setDouble(this, Double.parseDouble(pval));
			else if(t == Boolean.TYPE) {
				// if the provided value is actually 0 or 1, convert that to true or false
				if(pval.charAt(0) == '0') pval = "false";
				else if(pval.charAt(0) == '1') pval = "true";
				// then actually set the value
				f.setBoolean(this, Boolean.parseBoolean(pval));
			}
			else if(t == Character.TYPE) f.setChar(this, pval.charAt(0));
		} catch(NoSuchFieldException e) {
			// that's okay, the user can decide to do other things with it
			System.out.println("Unable to access " + pname + ".");
		} catch (IllegalAccessException e) {
			// this can also be okay, but should tell the user
			System.out.println("Unable to access " + pname + ".");
		} catch (NumberFormatException e) {
			// this is hypothetically okay, though it should tell the user
			System.out.println("Unable to set " + pname + " to " + pval + ": Number Format Exception!");
		}
	}

	/*
	 * writes results to file after the test interval
	 */
	public void writeResults(int s, String params, boolean end) {
		// first get the subclass and agent class
		setClasses();
		// surround it all with a try catch for the file writing
		try {
			// get whole model results (if any have been designated for collection)
			if(this.resnames.length > 0) {
				// initialize the result string
				String res = "";
				// loop through each result and get the value
				for(String r : this.resnames) {
					res += getResult(r, this, this.subclass) + this.sep;
				}
				// write the seed, timestep, params, and results to the timecourse results
				this.timewriter.write("" + s + this.sep + schedule.getSteps() + this.sep + params + res + "\n");
				// if this is the end of a run, also add it to end results
				if(end) {
					this.endwriter.write("" + s + this.sep + params + res + "\n");
				}
			}
			// and get individual agent results (if any have been designated, and this is the right interval)
			if(this.agentres.length > 0 && (this.agentint == 0 || this.schedule.getSteps()%this.agentint == 0)) {
				// loop through each agent in the list
				for(int o = 0; o < this.agents.length; o++) {
					// make sure the agent isn't null (there's no reason to print out all these empty lines)
					if(this.agents[o] != null) {
						// initialize a string for the results for this agent
						String res = "";
						//  loop through each result and get the corresponding value
						for(String r : this.agentres) {
							res += getResult(r, this.agents[o], this.agentclass) + this.sep;
						}
						// write params, the agent's number, and the results to the agent results
						this.agentwriter.write("" + s + this.sep + schedule.getSteps() + this.sep + params + o + this.sep + this.agents[o] + this.sep + res + "\n");
					}
				}
			}
			// also get network results (if any networks have been provided to test, and this is the right interval)
			if(this.nets.length > 0 && (this.netint == 0 || this.schedule.getSteps()%this.netint == 0)) {
				// for each network
				for(int i = 0; i < this.nets.length; i ++) {
					// this will all be under a try-catch because I have to use reflection to get the networks
					try {
						// this gets the field from the class
						Field f = this.subclass.getField(nets[i]);
						// and this grabs the actual network belonging to this object
						Network n = (Network) f.get(this);
						// make sure the network isn't null
						if(n != null) {
							// then I can go through the network and add all the edges to the file
							for(Edge[] edges : n.getAdjacencyList(true)) {
								for(Edge e : edges) {
									// make sure it's not trying to access a null edge, just in case
									if(e != null) {
										// add that edge to the file (along with all the other info about the run that it's part of)
										this.netwriters[i].write("" + s + this.sep + schedule.getSteps() + this.sep + params + e.getFrom() + this.sep + 
												e.getTo() + this.sep + e.getInfo() + this.sep + "\n");
									}
								}
							}
						}
					} catch(NoSuchFieldException|IllegalAccessException e) {
						System.out.println("Cannot access " + nets[i]);
					} catch(ClassCastException e) {
						System.out.println(nets[i] + " is not a network");
					}
				}
			}
			// and finally get list results
			if(this.listint == 0 || this.schedule.getSteps()%this.listint == 0) {
				// at the model level
				if(this.lists.length > 0) {
					// loop through each list and print it out
					for(String l : this.lists) {
						String lres = getResult(l, this, this.subclass);
						// write the seed, timestep, params, and results to the timecourse results
						this.listwriter.write("" + s + this.sep + schedule.getSteps() + this.sep + params + l + this.sep + lres + "\n");
					}
				}
				// and the agent level
				if(this.agentlists.length > 0) {
					for(String l : this.agentlists) {
						for(int i = 0; i < this.agents.length; i++) {
							if(this.agents[i] != null) {
								String lres = getResult(l, this.agents[i], this.agentclass);
								this.agentlistwriter.write("" + s + this.sep + schedule.getSteps() + this.sep + params + i + this.sep + this.agents[i] + this.sep + l + this.sep + lres + "\n");
							}
						}
					}
				}
			}
		} catch(IOException e) {
			System.out.println("Failed to write results to file...");
		}
	}
	
	/*
	 * Gets the value of a result parameter for a given object of a given class, and returns it as a string
	 * Should be modified for special cases in the subclass
	 */
	public String getResult(String res, Object o, Class c) {
		// initialize string to empty
		String p = "";
		// make sure the object isn't null
		if(o != null) {
		// start by checking to see if it's an actual field, the subclass will do something else if it wants
			try {
				Field f = c.getField(res);
				// get the value of that field
				Object val = f.get(o);
				// then check if it's a list to add its values
				if(val instanceof Collection) {
					p = Arrays.toString(((Collection)val).toArray());
				} else if(val instanceof Array) {
					p = Arrays.toString((Object[]) val);
				}else {
					// otherwise add it to the string directly
					p += f.get(o);
				}
				// things that don't handle this well will just have to be dealt with elsewhere
			} catch(NoSuchFieldException e) {
				// that's okay, this will just have to be dealt with in the subclass
			} catch(IllegalAccessException e) {
				// this is also hypothetically okay, but should tell the user
				System.out.println("Unable to access " + res + ".");
			}
		}
		return(p);
	}
	
	/*
	 * makes a template file that can then be read by the file based constructor
	 * Don't forget seed, steps, reps, filename, and testint
	 */
	public void makeInputFile() {
		try {
			// create a writer
			BufferedWriter writer = new BufferedWriter(new FileWriter("inputTemplate.txt"));
			// start with a 'brief' explanation of how to use the input file
			writer.write("% How to use:\n"
					+ "% This file allows you to set model parameter values for multiple simulations, and collect the results."
					+ " It automatically lists all primitive, String, and list type fields from your model class,"
					+ " as well as several key parameters (indicated by an asterisk '*') which are used for managing running and collecting data from the model.\n"
					+ "% To assign a value to a parameter, put the desired value after the equals sign."
					+ " Strings and characters will be read as is and should not be surrounded by quotation marks."
					+ " Each paramter can be assigned mutliple values to run a series of simulations with different parameter values as follows:\n"
					+ "%\tparameter = 0 1 2\n"
					+ "% If multiple parameters are each assigned multiple values, then simulations will be run for each combination of parameter values."
					+ " (You can assign as many values as you want to as many parameters as you want, but be careful as the number of simulations grows quickly.)\n"
					+ "% Parameter values can also be drawn randomly from a uniform (U - continuous, or C - discrete choices), normal (N), or gamma (G) distribution in the form:\n"
					+ "%\tparameter = U(<start>,<stop>)\n"
					+ "%\tparameter = C(<number of discrete options>)\n"
					+ "%\tparameter = N(<mean>,<standard deviation>)\n"
					+ "%\tparameter = G(<mean>,<standard deviation>,<optional minimum>)\n"
					+ "% To collect data on a field at the model level, leave the line empty as follows:\n"
					+ "%\tresult = \n"
					+ "% All fields that you don't want to set or collect, including key parameters, should be removed."
					+ " Only key parameters can use default values (where inidcated). All model parameters must be set in the input file."
					+ " All fields you want to set or collect must be made public."
					+ " You can add additional parameter and result names that are not fields to handle them manually in your model class."
					+ " All model-level results will be outputted in a  file named '<fname>endresults.txt' at the end of each simulation"
					+ " and in a file named '<fname>timeresults.txt' at set intervals throughout each simulation.\n"
					+ "% To collect data at the agent level, use the key parameter '*agentInfo',"
					+ " which is automatically followed by the names of all primitive, String, and list type fields of the agent class."
					+ " Keep the fields that you want to collect data on and delete those you do not want to collect data on."
					+ " You can also add additional result names that are not fields to handle them manually in your model class."
					+ " All agent-level results will be outputted in a file named '<fname>agentresults.txt' at set intervals throughout each simulation."
					+ " If you don't want to collect any agent-level data, delete the entire *agentInfo parameter.\n"
					+ "% To collect network data, use the key parameter '*edgeList',"
					+ " which is automatically followed by the names of all the Network type fields of your model class."
					+ " Keep the networks that you want to get an edgelist of and delete those you do not want an edgelist of."
					+ " The edgelists for each network will be outputted in files named <fname><network name>edgelist.txt at set intervals throughout each simulation."
					+ " If you don't want to collect any network data, delete the entire *edgeList parameter.\n"
					+ "% Comments (any text to be ignored when running the simulation) are indicated by the '%' character.\n\n");
			// then add the key parameters to the template, with comments
			writer.write("% Key Parameters:\n"
					+ "*seed =  % random seed used for the first replicate for each combination of parameter values (incremented for each additional replicate)\n"
					+ "*sep =  % separator character for the output file (defaults to comma separated)\n"
					+ "*steps =  % number of timesteps each simulation is run for\n"
					+ "*iters =  % number of sets of randomly drawn parameters\n"
					+ "*reps =  % number of simulations run for each combination of paramter values\n"
					+ "*fname =  % beginning of the names of all output files\n"
					+ "*testint =  % how often timecourse data is collected (in steps)\n"
					+ "*teststart =  % how many steps into each simulation to start collecting timecourse data, defaults to 0 (the beginning of the simulation)\n"
					+ "*gui =  % whether the simulation runs with or without GUI (defaults to false, only runs the initial set of parameter values if true)\n"
					+ "*agentint =  % how often agent-level data is collected (defaults to testint)\n"
					+ "*netint =  % how often edgelists are outputted (defaults to testint)\n"
					+ "*listint = % how often list-type data is outputted (defaults to testint)\n");
			// initialize the list of parameter names
			setNames();
			// then add all the listed input parameters
			writer.write("% Model Parameters:\n");
			for(int p = 0; p < paramnames.length; p++) {
				writer.write(paramnames[p] + " = \n");
			}
			// if parameter or result names are to be set automatically, also list all of the subclass's fields
			if(this.autoparams || this.autores) {
				// first store the subclass
				setClasses();
				// this will sore the list of networks
				String networks = "";
				Field[] fields = this.subclass.getDeclaredFields();
				for(int f = 0; f < fields.length; f++) {
					// make sure each field is a type it can handle
					Class c = fields[f].getType();
					if(c == String.class || c == Integer.TYPE || c == Double.TYPE || c == Character.TYPE || c == Boolean.TYPE || Collection.class.isAssignableFrom(c) || Array.class.isAssignableFrom(c))
						writer.write(fields[f].getName() + " = \n");
					else if(c == Network.class) {
						// also grab networks to suggest with edgelist
						networks += " " + fields[f].getName();
					}
				}
				// and then add agent info at the bottom
				writer.write("% Agent Parameters:\n");
				writer.write("*agentInfo = ");
				// with all of the agent's fields as suggested results (unless the agent class is null)
				if(this.agentclass != null) {
					fields = this.agentclass.getDeclaredFields();
					for(int f = 0; f < fields.length; f++) {
						// make sure each field is a type it can handle
						Class c = fields[f].getType();
						if(c == String.class || c == Integer.TYPE || c == Double.TYPE || c == Character.TYPE || c == Boolean.TYPE || Collection.class.isAssignableFrom(c) || Array.class.isAssignableFrom(c))
							writer.write(fields[f].getName());
					}
				}
				// also add edge list if there are networks
				if(networks.length() > 0) {
					writer.write("\n% Networks:\n");
					writer.write("*edgeList =" + networks);
				}
			}
			writer.close();
		} catch (IOException e) {
			System.out.println("Failed to create input file!");
		}
	}
	
	
	/*
	 * Initializer to split an input file into multiple input files by provided parameters
	 */
	public Model(String fname, String[] splitparams, String[] splitkeys) {
		super(0);
		// start by initializing parameters, results, and values using the pre-existing function
		String[] args = readFile(fname);
		splitFile(fname, "", splitparams, splitkeys, args);
	}
	
	/*
	 * Splits input file parsed into args (the output of readFile) based on provided parameters
	 */
	public void splitFile(String fname, String fext, String[] splitparams, String[] splitkeys, String[] args) {
		// if all elements have been removed from split params, create a file
		if(splitparams.length == 0) {
			try {
				// create a writer
				BufferedWriter writer = new BufferedWriter(new FileWriter(fext + fname));
				// start by adding the key parameters to the template
				for(int k = 0; k < keyparams.length; k++) {
					// make sure it isn't the filename, which will be handled separately
					if(!keyparams[k].equals("fname")) {
						try {
							writer.write("*" + keyparams[k] + " = " + Model.class.getField(keyparams[k]).get(this) + "\n");
						} catch (NoSuchFieldException e) {
							System.out.println("Tried to access a field that doesn't exist");
						} catch (IllegalAccessException e) {
							System.out.println("Tried to access a field that it doens't have access to");
						}
					}
				}
				// also add the file name
				writer.write("*fname = " + fext + this.fname + "\n");
				// now go through all of the normal parameters and put in their values
				for(int p = 0; p < paramnames.length; p++) {
					writer.write(paramnames[p] + " = " + args[p] + "\n");
				}
				// also add the results that were in the file, if they're going to be used
				if(this.autores) {
					for(int r = 0; r < fileres.length; r++) {
						writer.write(fileres[r] + " = \n");
					}
				}
				writer.close();
			} catch (IOException e) {
				System.out.println("Failed to create input file!");
			}
		} else {
			// otherwise, recurse on each value of the last split parameter
			// grab the index of that parameter in the param list
			int s = Arrays.asList(paramnames).indexOf(splitparams[0]);
			// grab the values of that parameter
			String[] vals = args[s].split(" ");
			// loop through them to recurse on each value
			for(int v = 0; v < vals.length; v++) {
				// create a copy of args with this value for the parameter
				String[] newargs = Arrays.copyOf(args, args.length);
				newargs[s] = vals[v];
				// recurse
				splitFile(fname, splitkeys[0] + vals[v] + fext,
						Arrays.copyOfRange(splitparams, 1, splitparams.length),
						Arrays.copyOfRange(splitkeys, 1, splitkeys.length), newargs);
			}
		}
	}
}
