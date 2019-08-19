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

import sim.engine.*;

public abstract class Model extends SimState  {

	// array to hold parameter values
	public String[] params;
	// initialize empty array of parameters to test
	public ArrayList<Integer> testparams = new ArrayList<Integer>();
	// initialize empty nested array of test parameter values
	public ArrayList<ArrayList<String>> testvals = new ArrayList<ArrayList<String>>();
	// file writer for the end of run results
	public BufferedWriter endwriter;
	// file writer for timecourse results taken at the indicated interval
	public BufferedWriter timewriter;
	// key parameters used in every model (as class variables for access if the user wants them)
	public static String[] keyparams = {"seed", "steps", "reps", "fname", "testint", "gui"};
	public int seed = 0;
	public int steps;
	public int reps = 1;
	public String fname = "";
	public int testint;
	public boolean gui = false;
	// list of parameter names - to be made in the child class
	public static String[] paramnames;
	// indicates whether to automatically use the subclass fields as parameters - default to true
	public boolean autoparams = true;
	// list of names of categories of data to gather - to be made in the child class
	public static String[] resnames;
	// indicates whether to automatically use the remaining subclass fields as results categories
	// defaults to true, but is only relevant when reading from file
	public boolean autores = true;
	// list of results names added from file for splitting
	public String[] fileres;
	// the subclass for use in accessing its fields
	public static Class subclass;

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
		setSubclass();
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
				// if there's something after the equals sign, add it to params
				if(splitline[1].length() > 1) {
					// first check for key parameters marked by a star
					if(line.charAt(0) == '*' && Arrays.asList(keyparams).contains(splitline[0].trim().substring(1))) {
						// modify the parameter's value
						setParamVal(Model.class, splitline[0].trim().substring(1), splitline[1].trim());
					} else {
						// add the first part to the list of parameter names (via tempparams)
						tempparams.add(splitline[0].trim());
						// and the second part to the list of parameter values (via tempvals)
						tempvals.add(splitline[1].trim());
					}
				} else if(splitline[1].length() <= 1 && this.autores) {
					// otherwise, if there's nothing after the equals sign
					// and it's supposed to automatically get the results categories, add those
					tempres.add(splitline[0].trim());
				}
			}
			// now these lists can become/be added to the official lists
			paramnames = tempparams.toArray(new String[tempparams.size()]);
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
				System.out.println("Missing key parameters for running without GUI "
						+ "(steps, reps, or testint)!");
				System.exit(0);
			}
			try {
				// files to write the results to
				this.endwriter = new BufferedWriter(new FileWriter(this.fname+"endresults.txt"));
				this.timewriter = new BufferedWriter(new FileWriter(this.fname + "timeresults.txt"));
				// write in a header
				makeHeader(this.endwriter, false);
				makeHeader(this.timewriter, true);
			} catch(IOException e) {
				System.out.println("Something's wrong with your results files!");
				System.exit(0);
			}
			try {
				// if there are no test params, just test
				if(this.testparams.size() == 0) {
					// test the model for the given seed, number of steps, replications,
					// and the test interval
					test(new ArrayList<String>(Arrays.asList(this.params)));
				} else {
					// otherwise sweep
					sweep(new ArrayList<String>(Arrays.asList(this.params)), 0);
				}
			} catch (NumberFormatException e) {
				System.out.println("Error in Runner: Constructor! Make sure all the parameters are the right type!");
				System.exit(0);
			}
			try {
				this.endwriter.close();
				this.timewriter.close();
			} catch (IOException e) {
				System.out.println("Writer not closing...");
			}
		}
	}

	/*
	 * parses inputed strings into lists of variables
	 */
	public void parse(int param, String val) {
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

	/*
	 * Writes the header for a results file
	 */
	public void makeHeader(BufferedWriter writer, boolean time) {
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
			// and a tab to make space for the seed
			writer.write("% \t");
			// if this file will hold time results, add an extra tab to make space for the time
			if(time) {
				writer.write("\t");
			}
			// indicate the test parameters if there are any
			if(this.testparams.size() > 0) writer.write("Parameters");
			// leave enough space for each parameter
			for(int t = 0; t < this.testparams.size(); t++) writer.write("\t");
			// then indicate categories for the results
			writer.write("Results\n");
			// start with the seed
			writer.write("Seed\t");
			// if this file will hold time results, add an extra column for the timestep
			if(time) {
				writer.write("Timestep\t");
			}
			// then loop through all the test parameters and add headers for each parameter
			for(int t = 0; t < this.testparams.size(); t++) {
				writer.write(this.paramnames[this.testparams.get(t)] + "\t");
			}
			// and add the headers for the actual categories of results
			for(int r = 0; r < this.resnames.length; r++) {
				writer.write(this.resnames[r] + "\t");
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
	public void sweep(ArrayList<String> params, int tdex) {
		// test each value of this parameter
		for(int v = 0; v < this.testvals.get(tdex).size(); v++) {
			// create a copy of params with this parameter value
			ArrayList<String> paramcopy = new ArrayList<String>(params);
			paramcopy.set(this.testparams.get(tdex), this.testvals.get(tdex).get(v));
			// if this isn't the last parameter, test the values of the remaining parameters
			if(tdex < this.testparams.size()-1) {
				sweep(paramcopy, tdex+1);
			} else {
				// otherwise, run this simulation
				test(paramcopy);
			}
		}
	}

	/*
	 * Actually runs the simulation on the provided parameters
	 */
	public void test(ArrayList<String> params) {
		System.out.println(params.toString());
		// set model parameters from args
		setParams(params.toArray(new String[params.size()]));
		// run the same simulation for the designated number of replications
		for(int i = 0; i < reps; i++) {
			// store the seed for this run
			int s = seed+i;
			// create an array to store the timecourse results
			double[][] timeres = new double[this.resnames.length][steps/testint];
			// reseed with the seed parameter, plus the replication number
			random.setSeed(s);
			// start the simulation
			start();
			// run the simulation for that number of steps
			while(schedule.getSteps() < steps) {
				// if this is the right step update the results array and add them to the time results array
				if(schedule.getSteps()%testint == 0) {
					// also print what step it is so I can keep track of progress
					// System.out.println(schedule.getSteps());
					double[] results = updateRes();
					for(int j = 0; j < resnames.length; j++) {
						timeres[j][(int) (schedule.getSteps()/testint)] = results[j];
					}
				}
				if (!schedule.step(this)) break;
			}
			finish();
			// try to write all those results to file
			try {
				// starting with the final population and learning results
				// first write the seed
				this.endwriter.write(s + "\t");
				// then write in all the test parameter values for this run
				for(int t = 0; t < this.testparams.size(); t++) {
					this.endwriter.write(params.get(this.testparams.get(t)) + "\t");
				}
				// then write the results from this run
				for(int r = 0; r < this.resnames.length; r++) {
					this.endwriter.write(timeres[r][steps/testint-1] + "\t");
				}
				// lastly, make a newline
				this.endwriter.write("\n");
				// and then do the timecourse results
				// start with the parameter values used for this run
				this.timewriter.write("% " + params.toString() + "\n");
				// and then write the parameters and results and for each step
				for(int t = 0; t < steps/testint; t++) {
					// first write the seed and the timestep
					this.timewriter.write(s + "\t" + testint*t + "\t");
					// then write the parameter values for this run (which will be the same on each line)
					for(int p = 0; p < this.testparams.size(); p++) {
						this.timewriter.write(params.get(this.testparams.get(p)) + "\t");
					}
					// then we can write the results
					for(int r = 0; r < this.resnames.length; r++) {
						this.timewriter.write(timeres[r][t] + "\t");
					}
					// and now go to the next line
					this.timewriter.write("\n");
				}
			} catch (IOException e) {
				System.out.println("Failed to write results to file...");
			}
		}
	}

	/*
	 * creates the lists of parameter names (paramnames) and result categories (resnames),
	 * can also change whether to automatically get params and results
	 */
	public void setNames() {
		// defaults to an empty list of parameters that will be filled in by the subclass's fields
		paramnames = new String[0];
		// and an empty list of result categories (will only be filled in if reading from file)
		resnames = new String[0];
	}

	/*
	 * The subclass has to set subclass to store its own type
	 */
	public abstract void setSubclass();

	/*
	 * takes an inputed list of strings and uses it to set parameter values
	 * the default is to use the parameter names as field names and assume they line up
	 */
	public void setParams(String[] params) {
		// get the subclass
		setSubclass();
		// loop through all named parameters
		for(int p = 0; p < paramnames.length; p++) {
			// if that parameter is an actual field, assign its value based on params
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
			if(t == String.class) f.set(this, pval);
			else if(t == Integer.TYPE) f.setInt(this, Integer.parseInt(pval));
			else if(t == Double.TYPE) f.setDouble(this, Double.parseDouble(pval));
			else if(t == Boolean.TYPE) f.setBoolean(this, Boolean.parseBoolean(pval));
			else if(t == Character.TYPE) f.setChar(this, pval.charAt(0));
		} catch(NoSuchFieldException e) {
			// that's okay, the user can decide to do other things with it
		} catch (IllegalAccessException e) {
			// this is also okay
		}
	}

	/*
	 * updates the results array at the end of each step (or when needed)
	 * the default assumes the names are the actual parameters
	 */
	public double[] updateRes() {
		// first get the subclass
		setSubclass();
		// initialize the list of results
		double[] results = new double[resnames.length];
		// then loop through all the results categories and check if any of them are actual fields
		for(int r = 0; r < resnames.length; r++) {
			try {
				Field f = subclass.getField(resnames[r]);
				results[r] = ((Number) f.get(this)).doubleValue();
			} catch(NoSuchFieldException e) {
				// that's okay, this will just have to be dealt with in the subclass
			} catch(IllegalAccessException e) {
				// this is also okay
			} catch(ClassCastException e) {
				// this ensures that the field is a number, if not it will have to be dealt with in the subclass
			}
		}
		return(results);
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
				setSubclass();
				Field[] fields = subclass.getDeclaredFields();
				for(int f = 0; f < fields.length; f++) {
					// make sure each field is a type it can handle
					Class c = fields[f].getType();
					if(c == String.class || c == Integer.TYPE || c == Double.TYPE || c == Character.TYPE || c == Boolean.TYPE)
						writer.write(fields[f].getName() + " = \n");
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
	 * Splits input file parsed into args based on provided parameters
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