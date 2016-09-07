package ie.ucd.mscba.practicum.fantasygolfga;

import ie.ucd.mscba.practicum.io.InputFile;
import ie.ucd.mscba.practicum.io.OutputFile;
import ie.ucd.mscba.practicum.rng.Rngs;

public class FantasyGolfGA {
	public static void main(String[] args) {
		
		// Read in Input/OutPut Files
		String inputFileName;
        String outputFileName;
        int numGolfersN = 0;
        Rngs rngs = new Rngs();
        rngs.putSeed(0);
        
        if ( args.length == 0 ) {
            inputFileName  = "practicum.in";
            outputFileName = "practicum.out";
        }
        else if ( args.length == 1 ) {
            inputFileName  = args[0];
            outputFileName = "practicum.out";
        }
        else {
            inputFileName  = args[0];
            outputFileName = args[1];
        }

        InputFile  inFile  = new InputFile(inputFileName);
        OutputFile outFile = new OutputFile(outputFileName);

        // Create and add our golfers
		TeamManager.getInputGolfers(inFile);
		        
        // Initialize population 1000
        Population pop = new Population(1000, true, rngs);
        
        //System.out.println("POP Size: " + pop.populationSize());
        //System.out.println("Initial PPM: " + pop.getFittest().getTotalPPM());
        //System.out.println("Initial Fittest: " + pop.getFittest());
                
        // Print Initial Population
        /*
        for (int i = 0; i < pop.populationSize(); i++) {
        	System.out.println("New Population: " + i + "= " + pop.getTeam(i));
        	System.out.println("Team Value: " + pop.getTeam(i).getTotalValue());
        }
        */
        // Evolve population for 1000 generations
        for (int i = 0; i <= 1000; i++) {
            pop = GA.evolvePopulation(pop, rngs);
            //System.out.println("Generation["+i+"] ======================================");
            System.out.println("Generation["+i+"] Solution:" + pop.getFittest() + "----- PPM: " + pop.getFittest().getTotalPPM() + "-----" + "Team Value: " + pop.getFittest().getTotalValue());
            //System.out.println("Solution:" + pop.getFittest() + "----- Average PPM: " + pop.getAverageFitness());
        }
        
       
	}

}
