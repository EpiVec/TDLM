/*
 *  Author: Maxime Lenormand (2023)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 */

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class TDM {

    public static void main(String[] args) throws FileNotFoundException {
    	
    	//Parameters: wdin, wdout, law, beta, pijonly, model, repli, writepij
    	String wdin = args[0];
    	String wdout = args[1];
    	String model = args[2];
    	double repli = Integer.parseInt(args[3]);
    	boolean multi = Boolean.parseBoolean(args[4]);
    	
        //Load data: pij, Oi and Dj      
        //Number of regions n
        int n = 0;
        Scanner scan = new Scanner(new File(wdin + "Mass.csv"));
        String[] cols = scan.nextLine().split(";");
        while (scan.hasNextLine()) {
        	cols = scan.nextLine().split(";");
            n++;
        }

        //Inputs
        double[] Oi = new double[n];  //Number of out-commuters (Oi) 
        double[] Dj = new double[n];  //Number of in-commuters (Dj)
        scan = new Scanner(new File(wdin + "Mass.csv"));
        scan.nextLine();
        int k = 0;
        while (scan.hasNextLine()) {
            cols = scan.nextLine().split(";");
            Oi[k] = Double.parseDouble(cols[2]);
            Dj[k] = Double.parseDouble(cols[3]);
            k++;
        }

        //Proba matrix pij (size n x n)
        double[][] pij = new double[n][n];
        scan = new Scanner(new File(wdin + "Proba.csv"));
        scan.nextLine();
        k = 0;
        while (scan.hasNextLine()) {
            cols = scan.nextLine().split(";");
            for (int i = 0; i < cols.length; i++) {
                cols[i] = cols[i].replace(',', '.');
                pij[k][i] = Double.parseDouble(cols[i]);
            }
            k++;
        }

        //Loop replications
        for (int r = 0; r < repli; r++) {

            //System.out.println("Replication " + (r + 1));

            //Simulated OD
            double[][] S = new double[n][n];

            //Network generation according to the constrained model 
            if (model.equals("UM")) {   //Unconstrained model
                S = UM(pij, Oi, multi);
            }
            if (model.equals("PCM")) {  //Production cconstrained model
                S = PCM(pij, Oi, multi);
            }
            if (model.equals("ACM")) {  //Attraction constrained model
                S = ACM(pij, Dj, multi);
            }
            if (model.equals("DCM")) {  //Doubly constrained model
                S = DCM(pij, Oi, Dj, 50, 0.01, multi);
            }

            //Write the resulting simulated OD matrix in a file 
            PrintWriter writer = new PrintWriter(new File(wdout + "S_" + (r + 1) + ".csv"));
            for (int j = 0; j < S.length; j++) {
                writer.print("V" + (j + 1));
                if(j < (S.length-1)) {
                    writer.print(";");
                }
            }
            writer.println();
            for (int i = 0; i < S.length; i++) {
                for (int j = 0; j < S.length; j++) {
                    writer.print(S[i][j]);
                    if(j < (S.length-1)) {
                        writer.print(";");
                    }
                }
                writer.println();
            }
            writer.close();
            
        }
        
    }

    //UM: generate the network using the Unconstrained Model
    //inputs: pij, Oi
    static double[][] UM(double[][] pij, double[] Oi, boolean multi) {

        int n = Oi.length;  //Number of Units

        //Number of commuters
        double nbCommuters = 0;
        for (int i = 0; i < n; i++) {
            nbCommuters += Oi[i];
        }

        //Sum pij for the normalization
        double sumt = 0.0;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                sumt += pij[i][j];
            }
        }

        //Sum pij by row for the Row normalization (see Multinomial_ij)
        double[] sum = new double[n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                sum[i] += pij[i][j];
            }
        }

        //NbCommuters are sampled from pij
        double[][] S = new double[n][n];
        if(multi) {
        	int nb = 0;
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    S[i][j] = Math.floor(nbCommuters * pij[i][j] / sumt);
                    nb += S[i][j];
                }
            }

            int[][] index = Multinomial_ij(((int) nbCommuters) - nb, pij, sum);
            for (int k = 0; k < index.length; k++) {
                S[index[k][0]][index[k][1]]++;
            }
        }else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    S[i][j] = nbCommuters * pij[i][j] / sumt;
                }
            }
        }

        return S;
    }

    //PCM: generate the network using the Production Constrained Model
    //inputs: pij, Oi
    static double[][] PCM(double[][] pij, double[] Oi, boolean multi) {

        int n = Oi.length; //Number of regions

        double[][] S = new double[n][n];

        //Sum pij for the normalization
        double[] sum = new double[n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                sum[i] += pij[i][j];
            }
        }

        //NbCommuters are sampled from pij preserving Oi
        if(multi) {
            int[] nb = new int[n];
            for (int i = 0; i < n; i++) {
                if (sum[i] > 0) {
                    for (int j = 0; j < n; j++) {
                        S[i][j] = Math.floor(Oi[i] * pij[i][j] / sum[i]);
                        nb[i] += S[i][j];
                    }
                }
            }
            for (int i = 0; i < n; i++) {
                if (Oi[i] != 0) {
                    int[] index = Multinomial_i(((int) Oi[i]) - nb[i], pij[i], sum[i]);
                    for (int k = 0; k < index.length; k++) {
                        S[i][index[k]]++;
                    }
                }
            }
        }else {
            for (int i = 0; i < n; i++) {
                if (sum[i] > 0) {
                    for (int j = 0; j < n; j++) {
                        S[i][j] = Oi[i] * pij[i][j] / sum[i];
                    }
                }
            }
        }

        return S;
    }

    //ACM: generate the network using the Attraction Constrained Model
    //inputs: pij, Dj
    static double[][] ACM(double[][] pij, double[] Dj, boolean multi) {

        int n = Dj.length; //Number of regions

        double[][] S = new double[n][n];

        //Transpose of pij
        double[][] tweights = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                tweights[i][j] = pij[j][i];
            }
        }

        //Sum pij for the normalization
        double[] sum = new double[n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                sum[i] += tweights[i][j];
            }
        }

        //NbCommuters are sampled from pij preserving Dj
        if(multi) {
            int[] nb = new int[n];
            for (int i = 0; i < n; i++) {
                if (sum[i] > 0) {
                    for (int j = 0; j < n; j++) {
                        S[j][i] = (int) Math.floor(Dj[i] * tweights[i][j] / sum[i]);
                        nb[i] += S[j][i];
                    }
                }
            }
            for (int i = 0; i < n; i++) {
                if (Dj[i] != 0) {
                    int[] index = Multinomial_i(((int) Dj[i]) - nb[i], tweights[i], sum[i]);
                    for (int k = 0; k < index.length; k++) {
                        S[index[k]][i]++;
                    }
                }
            }
        }else {
            for (int i = 0; i < n; i++) {
                if (sum[i] > 0) {
                    for (int j = 0; j < n; j++) {
                        S[j][i] = (int) Math.floor(Dj[i] * tweights[i][j] / sum[i]);
                    }
                }
            }
        }

        return S;
    }

    //DCM: generate the network using the Doubly Constrained Model
    //inputs: pij, Oi, Dj, maxIter (maximal number of iterations for the IPF procedure), closure (stopping criterion)
    static double[][] DCM(double[][] pij, double[] Oi, double[] Dj, int maxIter, double closure, boolean multi) {

        int n = Oi.length; //Number of Units

        //Iterative Proportional Fitting procedure (IPF)
        //IPF is a procedure for adjusting a table of data cells such that they add up to selected totals for both 
        //the columns and rows (in the two-dimensional case) of the table. 
        double[][] marg = new double[n][2];   //Observed marginals
        for (int i = 0; i < n; i++) {
            marg[i][0] = Oi[i];         //Observed marginal row       
            marg[i][1] = Dj[i];         //Observed marginal column 
            if (marg[i][0] == 0) {      //Only non-zero values
                marg[i][0] = 0.01;
            }
            if (marg[i][1] == 0) {      //Only non-zero values are admited
                marg[i][1] = 0.01;
            }
        }

        double[][] weights = new double[n][n];   //Seed of the IPF based on pij
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                weights[i][j] = pij[i][j];
                if (weights[i][j] == 0) {    //Only non-zero values are admited
                    weights[i][j] = 0.01;
                }
            }
        }

        int iter = 0;                   //Number of iterations
        double critOut = 1.0;           //Distance between observed and simulated marginal rows 
        double critIn = 1.0;            //Distance between observed and simulated marginal columns 
        double[] sout = new double[n];  //Simulated marginal rows 
        double[] sin = new double[n];   //Simulated marginal columns 

        //Repeat the process while iter lower than maxIter and the distances between observed and simulated marginal 
        //are above a threshold 
        while ((critOut > closure || critIn > closure) && (iter <= maxIter)) {

            //Compute sout
            for (int i = 0; i < n; i++) {
                sout[i] = 0;
                for (int k = 0; k < n; k++) {
                    sout[i] += weights[i][k];
                }
            }
            //Each row of weights is proportionally adjusted to equal the observed marginal row (specifically, each cell is divided 
            //by the simulated marginal row, then multiplied by the observed marginal row). 
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    weights[i][j] = marg[i][0] * weights[i][j] / sout[i];
                }
            }

            //Compute sin
            for (int i = 0; i < n; i++) {
                sin[i] = 0;
                for (int k = 0; k < n; k++) {
                    sin[i] += weights[k][i];
                }
            }
            //Each column of weights is proportionally adjusted to equal the observed marginal column (specifically, each cell is divided 
            //by the simulated marginal column, then multiplied by the observed marginal column).
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    weights[i][j] = marg[j][1] * weights[i][j] / sin[j];
                }
            }

            //Compute the distances between observed and simulated marginals
            critOut = 0.0;
            critIn = 0.0;
            for (int i = 0; i < n; i++) {
                sout[i] = 0;
                sin[i] = 0;
                for (int k = 0; k < n; k++) {
                    sout[i] += weights[i][k];
                    sin[i] += weights[k][i];
                }
                critOut = Math.max(critOut, Math.abs(1 - (sout[i] / marg[i][0])));
                critIn = Math.max(critIn, Math.abs(1 - (sin[i] / marg[i][1])));
            }
            iter++;
        }

        //NbCommuters are sampled from weights (or not)
        double[][] S = new double[n][n];
        if(multi) {
        	S = UM(weights, Oi, multi);
        }else {
        	S = weights;
        }
        
        return S;
    }

    //Multinomial_i: Given a vector of weights , it returns N indices randomly sampled according to these weights.
    //weights is a vector of weights
    //sum is the sum of weights (for the normalization)
    static int[] Multinomial_i(int n, double[] weights, double sum) {
        int[] randomIndex = new int[n];
        double[] random = new double[n];
        for (int k = 0; k < n; k++) {
            random[k] = Math.random() * sum;
        }
        for (int k = 0; k < n; k++) {
            for (int i = 0; i < weights.length; i++) {
                random[k] -= weights[i];
                if (random[k] <= 0.0) {
                    randomIndex[k] = i;
                    break;
                }
            }
        }
        return randomIndex;
    }

    //Multinomial_ij: Given a matrix of weights, it returns N 2D indices randomly sampled according to these weights.
    //weights is a matrix of weights
    //sum is vector of the sum by row (ie row marginal of the matrix)
    static int[][] Multinomial_ij(int n, double[][] weights, double[] sum) {

        int[][] randomIndex = new int[n][2];

        double sumt = 0.0;
        for (int k = 0; k < sum.length; k++) {
            sumt += sum[k];
        }

        double[] random = new double[n];
        double[] randomi = new double[n];
        for (int k = 0; k < n; k++) {
            random[k] = Math.random() * sumt;
            randomi[k] = random[k];
        }
        for (int k = 0; k < n; k++) {
            for (int i = 0; i < sum.length; i++) {
                randomi[k] -= sum[i];
                random[k] -= sum[i];
                if (randomi[k] <= 0.0) {
                    random[k] += sum[i];
                    randomIndex[k][0] = i;
                    break;
                }
            }
            for (int j = 0; j < weights.length; j++) {
                random[k] -= weights[randomIndex[k][0]][j];
                if (random[k] <= 0.0) {
                    randomIndex[k][1] = j;
                    break;
                }
            }
        }
        return randomIndex;
    }
}



