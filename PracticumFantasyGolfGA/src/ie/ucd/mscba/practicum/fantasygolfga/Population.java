package ie.ucd.mscba.practicum.fantasygolfga;

import ie.ucd.mscba.practicum.rng.Rngs;

public class Population {

    // Holds population of teams
    Team[] teams;

    // Construct a population
    public Population(int populationSize, boolean initialise, Rngs rngs) {
        teams = new Team[populationSize];
        // If we need to initialise a population of tours do so
        if (initialise) {
            // Loop and create individuals
            for (int i = 0; i < populationSize(); i++) {
                Team newTeam = new Team();
                newTeam.generateIndividual(rngs);
                saveTeam(i, newTeam);
            }
        }
    }
    
    // Saves a team
    public void saveTeam(int index, Team team) {
        teams[index] = team;
    }
    
    // Gets a team from population
    public Team getTeam(int index) {
        return teams[index];
    }

    // Gets the best team in the population
    public Team getFittest() {
        Team fittest = teams[0];
        int fittestInd = 0;
        int i = 0;
        // Loop through individuals to find fittest
        for (i = 1; i < populationSize(); i++) {
            if (fittest.getFitness() <= getTeam(i).getFitness()) {
                fittest = getTeam(i);
                fittestInd = i;
            }
        }
        swapFittest(0, fittestInd);
        return fittest;
    } // end getFittest()
    
    public Team getFittestAfterInd(int ind) {
        Team fittest = teams[ind];
        int fittestInd = 0;
        int i = 0;
        // Loop through individuals to find fittest
        for (i = ind; i < populationSize(); i++) {
            if (fittest.getFitness() <= getTeam(i).getFitness()) {
                fittest = getTeam(i);
                fittestInd = ind;
            }
        }
        swapFittest(0, ind);
        return fittest;
    } // getFittestAfterInd()
    
    public double getAverageFitness() {
    	double averageFitness = 0;
    	for (int i =0; i < populationSize(); i++) {
    		averageFitness += getTeam(i).getTotalPPM();
    	}
    	averageFitness = averageFitness/populationSize();
    	return averageFitness;
    } // end getAverageFitness()

    // Gets population size
    public int populationSize() {
        return teams.length;
    }
    
    public void swapFittest(int i, int fittest) {
    	Team temp = new Team();
    	temp = teams[i];
    	teams[i] = teams[fittest];
    	teams[fittest] = temp;
    }
    
}