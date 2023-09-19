# SimDataCollection

The biggest advantage of computer simulations is the ability to run an immense number of simulations and collect vast amounts of data. SimDataCollection enables you to do just that; it automates the process of running simulations with different combinations of parameter values and gathering the results for easy analysis in R or Python.

It is built around the SimDataCollection class, which extends the SimState class from the <a href="https://cs.gmu.edu/~eclab/projects/mason/">MASON Multi-Agent Toolkit</a>. To use it, extend SimDataCollection as you would SimState (see ModelTemplate.java in src/model for further guidance). Make sure that both MASON and SimDataCollection are in your project's buildpath. When you first run the simulation, SimDataCollection will automatically generate an input template file (see inputTemplate.txt for an example) which you can use to tell it what parameter values to run and what values to collect.
SimDataCollection makes it possible to run multiple simulations with all possible combinations of the desired parameter values, with multiple replicates per condition, and has additional options for randomly drawing parameter values from various distributions. It can gather data at the model (summary), agent, and network levels over the course of each simulation.

Additional, helper functions for drawing from various distributions are included in the distSampler and GammaNormalized classes.
An additional helper function for reading in networks from file is included in the NetworkLoader class.
Utilities for uploading and analyzing data in R are included in combineFiles.R (combines multiple results files into one data frame) and plotResults.R (recursively makes simple plots to see the results of combinations of parameter values).

This project is continuously under development. If you have any questions about how to use it, or if there are any utilities you would like to see included, please reach out to the developer at afblonder@ucdavis.edu.
