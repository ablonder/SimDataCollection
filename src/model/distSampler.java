package model;

import ec.util.MersenneTwisterFast;
import sim.util.distribution.Beta;
import sim.util.distribution.Distributions;

public class distSampler {
	
	/*
	 * helper utility to  draw a random value from a normal distribution within a certain range using a while-loop
	 * TODO - phase out
	 */
	public static double drawRange(MersenneTwisterFast random, double val, double var, double min, double max) {
		boolean within = false;
		double draw = 0;
		while(!within) {
			// draw variance from a normal distribution
			draw = random.nextGaussian()*var;
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
	public static double drawErlang(MersenneTwisterFast random, double mode, double sd) {
		// if the standard deviation is too low, just use a normal, that should be fine (otherwise it'll take a while and return infs)
		if(sd < .02) return drawRange(random, mode, sd, 0, Double.MAX_VALUE);
		double v = Math.pow(2*sd, 2);
		double m = (1+Math.sqrt(1 + 4*v))/2;
		// switch to draw from Gamma
		double draw = Distributions.nextErlang(v, m, random);
		if(draw == Double.POSITIVE_INFINITY) return mode;
		return draw*mode;
	}
	
	/*
	 * calls drawErlang with a min other than 0 by subtracting and then adding back
	 */
	public static double drawErlang(MersenneTwisterFast random, double mode, double sd, double min) {
		return drawErlang(random, mode-min, sd) + min;
	}
	
	/*
	 * Wrapper for GammaNormalized that adjusts the mean and min
	 */
	public static double drawGamma(MersenneTwisterFast random, double mean, double sd, double min) {
		GammaNormalized gamma = GammaNormalized.initialize(sd, random);
		return gamma.nextDouble()*(mean-min) + min;
	}
	
	/*
	 * helper utility to draw a random value from a nicely shaped Beta distribution between 0 and 1 (based around mode and "concentration")
	 */
	public static double drawBetaMode(MersenneTwisterFast random, double mode, double var) {
		double a = mode*(500*var)+1;
		double b = (1-mode)*(500*var)+1;
		Beta beta = new Beta(a, b, random);
		double draw = beta.nextDouble();
		return draw;
	}
	
	/*
	 * Draws from a beta distribution based on mean and inverse square root sample size (to allow for a parameter between 0 and 1)
	 */
	public static double drawBeta(MersenneTwisterFast random, double mean, double var) {
		// since this involves dividing by variation, make sure it's greater than 0
		if(var < .0001) return mean;
		// also make sure the mean is between .0001 and .9999 just to be safe
		mean = Math.min(Math.max(mean, .0001), .9999);
		double a = mean/Math.pow(var, 2);
		double b = (1-mean)/Math.pow(var, 2);
		Beta beta = new Beta(a, b, random);
		return beta.nextDouble();
	}
}
