package ie.ucd.mscba.practicum.fantasygolfga;

import ie.ucd.mscba.practicum.rng.Rngs;

import java.util.ArrayList;

public class Team{

    // Holds list of golfers
    private ArrayList team = new ArrayList<Golfer>();
    private double fitness = 0;									// fitness of team
    private double totalValue = 0;								// total value of team in millions
    private double totalPPM = 0;								// Points Per Million (PPM) is the form guide used. It is based on data mining and statistical analysis
    private int numGolfers = TeamManager.numberOfGolfers();
    private static Rngs r = new Rngs();							// Use the Rngs() class to utilise the Lehmer (Pseudo)Random Number Generator
    
	// Constructs a blank team
    public Team(){
        for (int i = 0; i < 10; i++) {
            team.add(null);
        }
    }
    
    public Team(ArrayList team){
        this.team = team;
    }

    // Create a random individual
    public void generateIndividual(Rngs rngs) {
    	r = rngs;
    	int selector = 0;
    	totalValue = 101;
    	while (totalValue > 100) {
    		// Loop through all our golfers and add them to our team
    		totalValue = 0;
            for (int i = 0; i < 10; i++) {
            	boolean individualSet = false;
    	        while (individualSet == false) {
    	        	selector = (int)(r.random()*numGolfers);
    	        	for (int j = 0; j < 10; j++) {
    	        		Golfer g = getGolfer(j);
    	        		if (g == TeamManager.getGolfer(selector)) {
    	        			individualSet = false;
    	        			selector = (int)(r.random()*numGolfers);
    	        			j = -1;
    	        		}
    	        		else {
    	        			individualSet = true;
    	        			
    	        		}
    	        			
    	        	}
    	        	
    	        	setGolfer(i, TeamManager.getGolfer(selector));
    	        	totalValue += TeamManager.getGolfer(selector).getGolferValue();
    	        		
    	        }
            }
    	}
        
    }

    // Gets a golfer from the team
    public Golfer getGolfer(int teamPosition) {
        return (Golfer)team.get(teamPosition);
    }

    // Sets a golfer in a certain position within a team
    public void setGolfer(int teamPosition, Golfer golfer) {
        team.set(teamPosition, golfer);
        // fitness = 0;
    }
    
    // Gets the team's fitness
    public double getFitness() {
        if (fitness >= 0) {
            fitness = getTotalPPM();
        }
        return fitness;
    }
    
    // Get the total value of the team
    public double getTotalValue() {
    	if (totalValue >= 0) {
    		double teamValue = 0;
    		// loop through players
    		for (int playerIndex = 0; playerIndex < teamSize(); playerIndex++){
    			Golfer g = getGolfer(playerIndex);
    			teamValue += g.getGolferValue();
    		}
    		totalValue = teamValue;
    	}
    	return totalValue;
    }
    
    // Get total PPM for team
    public double getTotalPPM() {
    	if (totalPPM >= 0) {
    		double teamPPM = 0;
    		// loop through players
    		for (int playerIndex = 0; playerIndex < teamSize(); playerIndex++){
    			Golfer g = getGolfer(playerIndex);
    			teamPPM += g.getGolferPPM();
    		}
    		totalPPM = teamPPM;
    	}
    	return totalPPM;
    }
    
    // Get number of golfers on team
    public int teamSize() {
        return team.size();
    }
    
    // Check if the team contains a golfer
    public boolean containsGolfer(Golfer golfer){
        return team.contains(golfer);
    }
    
    @Override
    public String toString() {
        String geneString = "|";
        for (int i = 0; i < teamSize(); i++) {
            geneString += getGolfer(i)+"|";
        }
        return geneString;
    }

	public void setTotalPPM(double totalPPM) {
		this.totalPPM = totalPPM;
	}
	
	 public int getNumGolfers() {
		return numGolfers;
	}

	public void setNumGolfers(int numGolfers) {
		this.numGolfers = numGolfers;
	}
    
}
