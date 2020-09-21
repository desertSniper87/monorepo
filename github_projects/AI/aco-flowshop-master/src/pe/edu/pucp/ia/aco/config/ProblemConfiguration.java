package pe.edu.pucp.ia.aco.config;

import isula.aco.Environment;
import isula.aco.algorithms.acs.AcsConfigurationProvider;
import isula.aco.algorithms.maxmin.MaxMinConfigurationProvider;
import isula.aco.exception.ConfigurationException;
import isula.aco.flowshop.AntForFlowShop;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Parameter settings for the algorithm.
 *
 * @author Carlos G. Gavidia (cgavidia@acm.org)
 * @author Adrian Pareja (adrian@pareja.com)
 */
public class ProblemConfiguration implements MaxMinConfigurationProvider,
        AcsConfigurationProvider {

    //  private static final String FILE_NAME = "flowshop_default.data";
    private static final String FILE_NAME = "ta001.data";

    private static final String FILE_FOLDER =
            "C:/Users/Carlos G. Gavidia/git/aco-flowshop/src/";

    public static final String FILE_DATASET = FILE_FOLDER + FILE_NAME;
    public static final int Q = 1;


    private static final int NUMBER_OF_ANTS = 1;
    private static final int VERY_IMPORTANT = 1;
    private static final int NOT_IMPORTANT = 0;

    private static final double EVAPORATION = 0.75;
    private static final int MAX_ITERATIONS = 20000;

    private Environment environment;
    private double initialPheromone = 1.0;

    public ProblemConfiguration(double[][] problemRepresentation) {
        List<Integer> randomSolution = new ArrayList<>();
        int numberOfJobs = problemRepresentation.length;
        for (int jobIndex = 0; jobIndex < numberOfJobs; jobIndex += 1) {
            randomSolution.add(jobIndex);
        }

        Collections.shuffle(randomSolution);
        double randomQuality = AntForFlowShop.getScheduleMakespan(
                randomSolution.toArray(new Integer[randomSolution.size()]),
                problemRepresentation);

        this.initialPheromone = 1 / (1 - EVAPORATION) / randomQuality;
    }

    public Environment getEnvironment() {
        return environment;
    }

    public void setEnvironment(Environment environment) {
        this.environment = environment;
    }

    public int getNumberOfAnts() {
        return NUMBER_OF_ANTS;
    }


    public double getEvaporationRatio() {
        return EVAPORATION;
    }

    public double getMaximumPheromoneValue() {
        throw new ConfigurationException(
                "We don't use this parameter in this version of the Algorithm");
    }

    public double getMinimumPheromoneValue() {
        throw new ConfigurationException(
                "We don't use this parameter in this version of the Algorithm");
    }

    public double getQValue() {
        return Q;
    }

    public int getBestChoiceConstant() {
        return 4;
    }

    public int getNumberOfIterations() {
        return MAX_ITERATIONS;
    }

    public double getInitialPheromoneValue() {
        return this.initialPheromone;
    }

    /**
     * This method of calculation is included in the paper.
     */
    public double getBestChoiceProbability() {
        double[][] problemGraph = this.environment.getProblemGraph();
        double bestChoiceProbability = (problemGraph.length - this
                .getBestChoiceConstant()) / problemGraph.length;
        return bestChoiceProbability;
    }

    public double getHeuristicImportance() {
        return NOT_IMPORTANT;
    }

    public double getPheromoneImportance() {
        return VERY_IMPORTANT;
    }
}
