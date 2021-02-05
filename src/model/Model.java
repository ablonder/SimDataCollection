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
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;

import ec.util.MersenneTwisterFast;
import sim.engine.*;
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
	// the separator for writing results to file
	public char sep = ',';
	// key parameters used in every model (as class variables for access if the user wants them)
	public static String[] keyparams = {"seed", "sep", "steps", "iters", "reps", "fname", "testint", "gui", "agentint"};
	public int seed = 0;
	public int steps;
	public int iters = 1;
	public int reps = 1;
	public String fname = "";
	public int testint;
	public boolean gui = false;
	public int agentint = 0;
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
	public String[] agentres;
	// list of agents
	public Object[] agents;
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
		// and an array list of results categories
		ArrayList<String> tempres = new ArrayList<String>();
		try {
			// now to open the file and start reading
			BufferedReader file = new BufferedReader(new FileReader(fname));
			// loop through the file
			String line;
			while((line = file.readLine()) != null) {
				// split each line at the equals sign
				String[] splitline = line.split("=");
				// first check for lines without anything after the equals, or no equals at all
				if(splitline.length < 2 || splitline[1].trim().length() < 1) {
					// if there's actually something there and we're automatically gathering results, do that
					if(this.autores && splitline[0].length() > 0) {
						tempres.add(splitline[0].trim());
					}
				} else if(this.autores && splitline[0].trim().equals("*agentInfo") && splitline[1].length() > 1) {
					// otherwise, try to catch agentInfo (which should be a key parameter)
					// split the parameters to get the data gathered from each agent
					ArrayList<String> newagentres = new ArrayList<String>(Arrays.asList(splitline[1].trim().split(" ")));
					// grab the existing list of agent parameters
					ArrayList<String> oldagentres = new ArrayList<String>(Arrays.asList(this.agentres));
					// concatenate the two
					oldagentres.addAll(newagentres);
					// store the resulting complete list as an array
					this.agentres = oldagentres.toArray(new String[oldagentres.size()]);
				} else if(splitline[1].length() > 1) {
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
			} catch(IOException e) {
				System.out.println("Something's wrong with your results files!");
				System.exit(0);
			}
			// draw random parameters for a set number of iterations (from a separate random seed), and then sweep/test in each
			MersenneTwisterFast paramgen = new MersenneTwisterFast();
			paramgen.setSeed(this.seed);
			for(int i = 0; i < this.iters; i++) {
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
				if(distparams.length < 3) return(drawGamma(Double.parseDouble(distparams[0]), Double.parseDouble(distparams[1]), 0, rand));
				return drawGamma(Double.parseDouble(distparams[0]), Double.parseDouble(distparams[1]), Double.parseDouble(distparams[2]), rand);
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
				writer.write("Agent" + this.sep);
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
				// TODO - give it the option to delay when it starts collecting data
				if(schedule.getSteps()%testint == 0) {
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
		} catch (IllegalAccessException e) {
			// this is also okay
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
						this.agentwriter.write("" + s + this.sep + schedule.getSteps() + this.sep + params + o + this.sep + res + "\n");
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
				// things that don't handle this well will just have to be dealt with elsewhere
				p += f.get(o);
			} catch(NoSuchFieldException e) {
				// that's okay, this will just have to be dealt with in the subclass
			} catch(IllegalAccessException e) {
				// this is also okay
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
			// start by adding the key parameters to the template
			for(int k = 0; k < keyparams.length; k++) {
				writer.write("*" + keyparams[k] + " = \n");
			}
			// initialize the list of parameter names
			setNames();
			// then add all the listed input parameters
			for(int p = 0; p < paramnames.length; p++) {
				writer.write(paramnames[p] + " = \n");
			}
			// if parameter or result names are to be set automatically, also list all of the subclass's fields
			if(this.autoparams || this.autores) {
				// first store the subclass
				setClasses();
				Field[] fields = this.subclass.getDeclaredFields();
				for(int f = 0; f < fields.length; f++) {
					// make sure each field is a type it can handle
					Class c = fields[f].getType();
					if(c == String.class || c == Integer.TYPE || c == Double.TYPE || c == Character.TYPE || c == Boolean.TYPE)
						writer.write(fields[f].getName() + " = \n");
				}
				// and then add agent info at the bottom
				writer.write("*agentInfo = ");
				// with all of the agent's fields as suggested results
				fields = this.agentclass.getDeclaredFields();
				for(int f = 0; f < fields.length; f++) {
					writer.write(fields[f].getName() + " ");
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
	
	/*
	 * helper utility to  draw a random value from a normal distribution within a certain range using a while-loop
	 * TODO - phase out
	 */
	public double drawRange(double val, double var, double min, double max) {
		boolean within = false;
		double draw = 0;
		while(!within) {
			// draw variance from a normal distribution
			draw = this.random.nextGaussian()*var;
			// check to see if its in the right range;
			if(val+draw >= min && val+draw <= max) {
				within = true;
			}
		}
		// once a reasonable value has been found, return it
		return draw+val;
	}
	
	/*
	 * helper utility to draw a random value from a nicely shaped Erlang distribution (based around mode and standard deviation)
	 * 	(note: struggles with large values - algorithm runs mean^2/var iterations
	 *   scale by dividing mean and sd by a constant, convert sd to variance by squaring, and then scale back up the result)
	 *   TODO - phase out for GammaNormalized
	 */
	public double drawErlang(double mode, double sd) {
		// if the standard deviation is too low, just use a normal, that should be fine (otherwise it'll take a while and return infs)
		if(sd < .02) return drawRange(mode, sd, 0, Double.MAX_VALUE);
		double v = Math.pow(2*sd, 2);
		double m = (1+Math.sqrt(1 + 4*v))/2;
		// switch to draw from Gamma
		double draw = Distributions.nextErlang(v, m, this.random);
		if(draw == Double.POSITIVE_INFINITY) return mode;
		return draw*mode;
	}
	
	/*
	 * calls drawErlang with a min other than 0 by subtracting and then adding back
	 */
	public double drawErlang(double mode, double sd, double min) {
		return drawErlang(mode-min, sd) + min;
	}
	
	/*
	 * Wrapper for GammaNormalized that adjusts the mean and min
	 */
	public double drawGamma(double mean, double sd, double min, MersenneTwisterFast random) {
		GammaNormalized gamma = GammaNormalized.initialize(sd, random);
		return gamma.nextDouble()*(mean-min) + min;
	}
	
	/*
	 * option to run Gamma with the default random generator
	 */
	public double drawGamma(double mean, double sd, double min) {
		return drawGamma(mean, sd, min, this.random);
	}
	
	/*
	 * helper utility to draw a random value from a nicely shaped Beta distribution between 0 and 1 (based around mode and "concentration")
	 * TODO - phase out
	 */
	public double drawBetaMode(double mode, double var) {
		double a = mode*(500*var)+1;
		double b = (1-mode)*(500*var)+1;
		Beta beta = new Beta(a, b, this.random);
		double draw = beta.nextDouble();
		return draw;
	}
	
	/*
	 * Draws from a beta distribution based on mean and inverse square root sample size (to allow for a parameter between 0 and 1)
	 */
	public double drawBeta(double mean, double var) {
		// since this involves dividing by variation, make sure it's greater than 0
		if(var < .0001) return mean;
		// also make sure the mean is between .0001 and .9999 just to be safe
		mean = Math.min(Math.max(mean, .0001), .9999);
		double a = mean/Math.pow(var, 2);
		double b = (1-mean)/Math.pow(var, 2);
		Beta beta = new Beta(a, b, this.random);
		return beta.nextDouble();
	}
	

}