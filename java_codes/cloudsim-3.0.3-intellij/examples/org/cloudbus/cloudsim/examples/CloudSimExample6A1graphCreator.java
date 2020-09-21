/*
 * Title:        CloudSim Toolkit
 * Description:  CloudSim (Cloud Simulation) Toolkit for Modeling and Simulation
 *               of Clouds
 * Licence:      GPL - http://www.gnu.org/copyleft/gpl.html
 *
 * Copyright (c) 2009, The University of Melbourne, Australia
 */


package org.cloudbus.cloudsim.examples;

import org.cloudbus.cloudsim.*;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RefineryUtilities;

import java.awt.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * An example showing how to create
 * scalable simulations.
 */
public class CloudSimExample6A1graphCreator extends ApplicationFrame {

    private static XYSeriesCollection dataSetCloudletVsCloudletID;
    private static XYSeries seriesVMVsExecutionTime = new XYSeries("A1");

    private static List<Cloudlet> receivedCloudletList;
    private static List<Double> finishTimeList;

    public CloudSimExample6A1graphCreator(String title) {
        super(title);
        JFreeChart chartCloudletVsCloudletID = ChartFactory.createXYLineChart(
                "Finishing Time vs number of VMs", // Title
                "Number of VMs", // x-axis Label
                "Finishing Time", // y-axis Label
                dataSetCloudletVsCloudletID, // Dataset
                PlotOrientation.VERTICAL, // Plot Orientation
                true, // Show Legend
                true, // Use tooltips
                false // Configure chart to generate URLs?
        );

        ChartPanel chartPanelCloudletVsCloudletID = new ChartPanel(chartCloudletVsCloudletID);
//        chartPanelCloudletVsCloudletID.setPreferredSize( new java.awt.Dimension( 560 , 367 ) );
        chartPanelCloudletVsCloudletID.setPreferredSize(new Dimension(800, 600));
        setContentPane(chartPanelCloudletVsCloudletID);
        XYPlot plot = (XYPlot) chartCloudletVsCloudletID.getPlot();
        plot.getRenderer().setSeriesPaint(0, Color.RED);
    }







    ////////////////////////// STATIC METHODS ///////////////////////

    /**
     * Creates main() to run this example
     */
    public static void main(String[] args) {
        Log.printLine("Starting CloudSimExample6...");

        int cloudlets = 4000;

        try {
            // First step: Initialize the CloudSim package. It should be called
            // before creating any entities.
            for (int i = 4; i < 35; i++) {
                List<Double> finishTimeList = new ArrayList<Double>();
                CloudSimExample6A1vanilla cloudSimulator = new CloudSimExample6A1vanilla();
                cloudSimulator.setVMCloudlet(i, cloudlets);
                cloudSimulator.main(new String[]{});
                receivedCloudletList = cloudSimulator.getList();
                //seriesVMVsExecutionTime.add(i, receivedCloudletList.get(cloudlets-1).getFinishTime());
                for (Cloudlet cloudlet: receivedCloudletList)
                    finishTimeList.add(cloudlet.getFinishTime());

                String maxFinishTime = Collections.max(finishTimeList).toString();
                try(FileWriter fw = new FileWriter("Vm VS finishTime A1", true);
                    BufferedWriter bw = new BufferedWriter(fw);
                    PrintWriter out = new PrintWriter(bw))
                {
                    out.print(String.valueOf(i));
                    out.print("\t");
                    out.println(maxFinishTime);
                } catch (IOException e) {
                    //Halp
                }

                seriesVMVsExecutionTime.add(i, Collections.max(finishTimeList));
            }

            dataSetCloudletVsCloudletID = new XYSeriesCollection();
            dataSetCloudletVsCloudletID.addSeries(seriesVMVsExecutionTime);
            CloudSimExample6A1graphCreator chart = new CloudSimExample6A1graphCreator("Finishing time vs. number of VMs");
            chart.pack();
            RefineryUtilities.centerFrameOnScreen(chart);
            chart.setVisible(true);

            Log.printLine("CloudSimExample6 finished!");
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Log.printLine("The simulation has been terminated due to an unexpected error");
        }
    }

}
