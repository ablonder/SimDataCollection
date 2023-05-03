package model;

/*
 * A template with the functions necessary for building a model to extend the Model class
 */
public class ModelTemplate extends SimDataCollection{
	
	// All fields that you want to vary or output need to be public so that can be accessed by the Model class 
	
	
	/*
	 * Empty constructor to generate an input template file (inputTemplate.txt)
	 */
	public ModelTemplate(){
		super();
	}
	
	/*
	 * Constructor to run the simulation from an input file (fname) and collect data
	 */
	public ModelTemplate(String fname) {
		super(fname);
	}
	
	/*
	 * Constructor to split one input file (fname) into multiple input files,
	 * one for each combination of values of the given parameters (params)
	 */
	public ModelTemplate(String fname, String[] params, String[] paramkeys) {
		super(fname, params, paramkeys);
	}
	
	/*
	 * Implement an abstract method from the Model class to tell it the model class and agent class
	 */
	public void setClasses() {
		this.subclass = ModelTemplate.class;
		// TODO - set this.agentclass = <Agent>.class
	}
	
	/*
	 * Implement abstract start method from MASON's SimState to initialize each simulation
	 */
	public void start() {
		super.start();
	}
	
	/*
	 * Optional!
	 * Overrides Model#setParamVal to set parameter values from file for those that need to be handled manually
	 */
	public void setParamVal(Class c, String pname, String pval) {
		// TODO - assign values to parameter pname based on the value provided from file pval
		// the Model class handles all simple parameters that are just a field of the model class
		super.setParamVal(c, pname, pval);
	}
	
	
	/*
	 * Optional!
	 * Overrides Model#getResult to return the value for any output measures that need to be handled manually
	 */
	public String getResult(String r, Object o, Class c) {
		// to handle results at the model level
		if(c == this.subclass) {
			switch(r) {
				// TODO - return the value of output r of the model class
			}
		} else if(c == this.agentclass) {
			// to handle results at the agent level
			switch(r) {
				// TODO - return the value of output r of the agent class
			}
		}
		// the Model class handles all simple output measures that are just a field of the model or agent class
		return(super.getResult(r, o, c));
	}
	
	/*
	 * Main method for running the simulation and collecting data
	 */
	public static void main(String[] args) {
		// TODO - to get the input template - new ModelTemplate()
		// TODO - to run a simulation from an input file - new ModelTemplate(<input file name>)
		// or with an input file name as an argument - new ModelTemplate(args[0])
	}

}
