package model;

import ec.util.MersenneTwisterFast;
import sim.util.distribution.Gamma;
/*
 * This class extends the Gamma class in Mason to a normalized version for a mean of 1.0 that generates
 * random Gamma deviates with a specified standard deviation (sd).  The mean of a Gamma distribution lambda/alpha
 * and the variance is Lambda/alpha^2.  The standard deviation is thus (Lambda/alpha^2)^(1/2).  Assuming the standard
 * deviations is 1, (Lambda/alpha^2)^(1/2), lambda = alpha^2.  Setting alpha = 1.0/(sd*sd) = lambda yield Gamma random
 * deviates with a mean of 1.0 and variance lambda/alpha^2 and the standard deviation is (Lambda/alpha^2)^(1/2).  For example, suppose
 * we want to generate random Gamma deviates with a mean of 1 and sd = 0.1.  We set alpha = 1/(0.1*0.1) = 100 and set lambda = alpha = 100.
 * The mean is lambda/alpha = 100/100 = 1.  The variance  is lambda/alpha^2 = 100/10000 = 0.01.  The standard deviation is (Lambda/alpha^2)^(1/2) =
 * 0.01^(1/2) = 0.1.
 */
public class GammaNormalized extends Gamma {
	public boolean sdZero = false;

	public GammaNormalized(double alpha, double lambda, MersenneTwisterFast randomGenerator) {
		super(alpha, lambda, randomGenerator);
	}
	
	public static GammaNormalized initialize(double sd, MersenneTwisterFast randomGenerator) {
		if(sd == 0) {
			GammaNormalized gd = new GammaNormalized(1.0, 1.0, randomGenerator);
			gd.sdZero = true;
			return gd;
		}
		final double  alpha = 1.0/(sd*sd);
		final double  lambda = alpha;
		return new GammaNormalized(alpha, lambda, randomGenerator);
	}
	
	public double nextDouble() {
		if(sdZero) {
			return 1.0;
		}
		return super.nextDouble();
	}

}
