package ie.ucd.mscba.practicum.fantasygolfga;

public class Golfer {
    double golferID;
    double golferValue;
    double golferPPM;
    int numGolfers = 0;
    
    // Constructs a random golfer
    public Golfer(){
        this.golferID = (int)(Math.random());
        this.golferValue = (int)(Math.random());
        this.golferPPM = (int)(Math.random());
    }
    
    // Constructs a golfer
    public Golfer(double ID, double val, double ppm){
        this.golferID = ID;
        this.golferValue = val;
        this.golferPPM = ppm;
    }
    
    // Gets golfer's ID
    public double getGolferID(){
        return this.golferID;
    }
    
    // Gets golfer's value
    public double getGolferValue(){
        return this.golferValue;
    }
    
 // Gets golfer's PPM
    public double getGolferPPM(){
        return this.golferPPM;
    }
    
 // Gets number of golfers
    public int getNumGolfers(){
        return this.numGolfers;
    }
    
     
    @Override
    public String toString(){
        return getGolferID() + "=ID";
    }
}
