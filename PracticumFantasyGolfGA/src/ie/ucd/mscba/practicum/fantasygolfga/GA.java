package ie.ucd.mscba.practicum.fantasygolfga;

import ie.ucd.mscba.practicum.rng.Rngs;

public class GA {

    /* GA parameters */
	private static final double crossoverRate = 0.75; 				// 0.75 = 75% Crossover Rate
    private static final double mutationRate = 0.1;					// 0.1 = 10% Mutation Rate		
    private static final int tournamentSize = 2;					// 2 teams in a tournament and select best one for reproduction
    private static final boolean elitism = true;					// Use elitism
    private static int numElites = 2;								// Keep the 2 best candidates - elites
    private static int crossoverPoint = 0;							// The Point at which one-point crossover occurs
    private static int mutationPoint = 0;							// The gene/golfer that will be mutated
    private static Rngs r = new Rngs();								// Use the Rngs() class to utilise the Lehmer (Pseudo)Random Number Generator
    // private static int seed = 444444;								// Set the initial seed for Random Number Generator - this is more "random" then built-in java random
    
   
    // Evolves a population over one generation
    public static Population evolvePopulation(Population pop, Rngs rngs) {
        Population newPopulation = new Population(pop.populationSize(), false, rngs);		// create a blank Population
        
        r = rngs;
        //r.putSeed(seed);											// Set RNG seed

        // Keep our best individual(s) if elitism is enabled
        Population elitesPopulation = saveElites(pop);				// save elites separately
        newPopulation.saveTeam(0, elitesPopulation.getTeam(0));		// Set first team of new population to be best team of the previous generation
        newPopulation.saveTeam(1, elitesPopulation.getTeam(1));		// Set second team of new population to be second best team of previous generation
        numElites = 2;
        int i = numElites;											// Start i at numElites and work on the remaining proportion of the population using crossover and mutation
        
        while (i < newPopulation.populationSize()) {  				// Using the population minus the elites
        	// Select parents
            Team parent1 = tournamentSelection(pop);
            Team parent2 = tournamentSelection(pop);
            // Create blank children
            Team child1 = new Team();
            Team child2 = new Team();
            
            if (parent1.getTotalValue() > 100) {
            	parent1.generateIndividual(r);
            }
            if (parent2.getTotalValue() > 100) {
            	parent2.generateIndividual(r);
            }
            
            
            // Apply Crossover Probability
            if(r.random() < crossoverRate) {
	            // Crossover parents to create new children
            	crossoverPoint = (int) (r.random() * parent1.teamSize());
            	child1 = crossover(parent1, parent2, crossoverPoint);
	            child2 = crossover(parent2, parent1, crossoverPoint);
	            // If children are invalid teams then reset to parents
	            if (child1.getTotalValue() > 100 || child2.getTotalValue() > 100) {
	            	child1 = parent1;
		            child2 = parent2;
	            }
	            
	        }
            // no crossover so children are same as parents
            else {
	           	child1 = parent1;
	            child2 = parent2;
	        }
            // Add children to new population
            newPopulation.saveTeam(i, child1);
            i += 1;
            newPopulation.saveTeam(i, child2);
            i += 1;
        }
        //System.out.println("CROSSOVER COMPLETE");
        
        // Mutate the new population a bit to add some new genetic material
        for (int e = numElites; e < newPopulation.populationSize(); e++) {
            mutate(newPopulation.getTeam(e));
        }
        //System.out.println("MUTATION COMPLETE");
        
        
        while (newPopulation.getFittest().getTotalValue() > 100.0) {
        	newPopulation.getFittest().generateIndividual(r);
        }
        
        return newPopulation;
    } // end evolvePopulation()

    // Applies crossover to a set of parents and creates offspring
    public static Team crossover(Team parent1, Team parent2, int one_point) {
        // Create new blank child
    	Team child = new Team();

        // Using point where crossover will be applied
        // 1-point Crossover
        for (int i = 0; i < one_point; i++) {
        	child.setGolfer(i, parent1.getGolfer(i));
        }
        for (int i = one_point; i < parent2.teamSize(); i++) {
        	if (!child.containsGolfer(parent2.getGolfer(i))){
        		child.setGolfer(i, parent2.getGolfer(i));
        	}
        	else if (child.containsGolfer(parent1.getGolfer(i))) {
        		child = parent1;
        	}
        	else {
        		child.setGolfer(i, parent1.getGolfer(i));
        	}
        	
        }
        return child;
    } // end crossover()

    // Mutate a team using gene (golfer) mutation
    private static void mutate(Team team) {
        // If mutationRate then randomly select a golfer and mutate to random substitute
    	// maintaining a total team value < 100
    	if(r.random() < mutationRate) {
    		mutationPoint = equilikely(0,team.teamSize()-1,r);
    		Golfer oldGolfer = team.getGolfer(mutationPoint);
    		double newGolferID = equilikely(0,TeamManager.numberOfGolfers()-1,r);
    		Golfer newGolfer = TeamManager.getGolfer(newGolferID);
    		while (team.containsGolfer(newGolfer)){
    			newGolferID = equilikely(0,TeamManager.numberOfGolfers()-1,r);
        		newGolfer = TeamManager.getGolfer(newGolferID);
    		}
			team.setGolfer(mutationPoint, newGolfer);
    		
			if (team.getTotalValue() > 100) {
    			team.setGolfer(mutationPoint, oldGolfer);
    		}
    	}
    	
    } // end mutate()

    // Selects candidate team for crossover
    private static Team tournamentSelection(Population pop) {
        // Create a tournament population
        Population tournament = new Population(tournamentSize, false, r);
        // For each place in the tournament get a random candidate team and add it
        for (int i = 0; i < tournamentSize; i++) {
            int randomId = (int) (r.random() * pop.populationSize());
            tournament.saveTeam(i, pop.getTeam(randomId));
        }
        // Get the fittest team
        Team fittest = tournament.getFittest();
        return fittest;
    } //end tournamentSelection()
    
    // Select elites and save them
    private static Population saveElites(Population oldPop){
    	Population elites = new Population(numElites, false, r);
    	if (elitism) {
    		elites.saveTeam(0, oldPop.getFittest());
            elites.saveTeam(1, oldPop.getFittestAfterInd(1));
        }
    	return elites;
    } // end saveElites()
    
    
    // equilikely function that returns a random int between a and b
    private static int equilikely(long a, long b, Rngs r) {
    	r.selectStream(5);
        return (int) (a + (long) ((b - a + 1) * r.random()));
    } // end equilikely
}
