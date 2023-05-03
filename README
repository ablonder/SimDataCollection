# SimDataCollection

This repository contains a utility for collecting data from the MASON Multi-Agent Simulation Toolkit.

It is built around the SimDataCollection class, which extends the SimState class from MASON. SimDataCollection reads in parameter values and output measures from a file, runs all of the corresponding simulations, and outputs the results in an R-friendly format. It enables you to run multiple simulations with all possible combinations of desired parameter values, with multiple replicates per condition, and has additional options for randomly drawing parameter values from various distributions. It automatically gathers data at the model, agent, and network levels over the course of each simulation, with the option for additional, manually calculated measures.
Additional, helper functions for drawing from various distributions are included in the distSampler and GammaNormalized classes.
An additional helper function for reading in networks from file is included in the NetworkLoader class.
Utilities for uploading and analyzing data in R are included in combineFiles.R (combines multiple results files into one data frame) and plotResults.R (recursively makes simple plots to see the results of combinations of parameter values).

To use, extend the SimDataCollection class as you would extend SimState to create a simulation using MASON (see the ModelTemplate class). Make sure that both MASON and SimDataCollection are in your project's buildpath.
To generate a template input file (a file named "inputTemplate.txt" created in your project folder), run your simulation with no arguments.

This project is continuously under development. If you have any questions about how to use it, or if there are any utilities you would like to see included, please reach out to the developer at afblonder@ucdavis.edu.
