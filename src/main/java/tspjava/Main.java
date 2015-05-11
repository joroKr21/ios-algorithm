package tspjava;

import org.coinor.opents.*;

public class Main 
{

    public static void main (String args[]) 
    {
        // Initialize our objects
        java.util.Random r = new java.util.Random( 12345 );
        double[][] customers = new double[20][2];
        double[] left = new double[] {72, 40, 31, 20, 66, 89, 75, 47, 38, 17, 31, 79, 139, 189, 166, 183, 162, 119, 20, 18 };
        double[] right = new double[]{186, 170, 153, 188, 138, 127, 99, 69, 50, 37, 13, 46, 9, 13, 65, 70, 95, 182, 118, 111 };


//        for( int i = 0; i < 20; i++ )
//            for( int j = 0; j < 2; j++ )
//                customers[i][j] = r.nextDouble()*200;

        for( int i = 0; i < 20; i++ ) {
            customers[i][0] = left[i];
            customers[i][1] = right[i];
        }
        
        ObjectiveFunction objFunc = new MyObjectiveFunction( customers );
        Solution initialSolution  = new MyGreedyStartSolution( customers );
        MoveManager   moveManager = new MyMoveManager();
        TabuList         tabuList = new SimpleTabuList( 7 ); // In OpenTS package
        
        // Create Tabu Search object
        TabuSearch tabuSearch = new SingleThreadedTabuSearch(
                initialSolution,
                moveManager,
                objFunc,
                tabuList,
                new BestEverAspirationCriteria(), // In OpenTS package
                false ); // maximizing = yes/no; false means minimizing
        
        // Start solving
        tabuSearch.setIterationsToGo( 100 );
        tabuSearch.startSolving();
        
        // Show solution
        MySolution best = (MySolution)tabuSearch.getBestSolution();
        System.out.println( "Best Solution:\n" + best );

        int[] tour = best.tour;
        for( int i = 0; i < tour.length; i++ )
            System.out.println( 
             customers[tour[i]][0] + "\t" + customers[tour[i]][1] );
    }   // end main

}   // end class Main
