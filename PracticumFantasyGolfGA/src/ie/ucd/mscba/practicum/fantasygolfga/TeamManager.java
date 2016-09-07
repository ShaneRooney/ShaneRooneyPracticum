package ie.ucd.mscba.practicum.fantasygolfga;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.StringTokenizer;

import ie.ucd.mscba.practicum.io.InputFile;
import ie.ucd.mscba.practicum.io.OutputFile;

public class TeamManager {

    // Holds our teams
    private static ArrayList golfers = new ArrayList<Golfer>();
    // Use these files to read in list of golfers
    private OutputFile outFile;   					
	private InputFile inFile;
	private Formatter formatter = new Formatter();
	private String errorMessage = "Cannot proceed";
	//private int numGolfers = 0;
	
	
    // Adds a golfer
    public static void addGolfer(Golfer golfer) {
        golfers.add(golfer);
    }
    
    // Get a golfer
    public static Golfer getGolfer(double index){
        return (Golfer)golfers.get((int) index);
    }
    
    // Get the number of golfers
    public static int numberOfGolfers(){
        return golfers.size();
    }
    
 // get NumGolfers from Input - first line
    public static int getNumGolfersFromFile(InputFile inFile) {
    	
    	int n = 0;
		try {
			String line = inFile.readLine();	// a reference to the most recent String returned by inFile.readLine()
			String token;     	// a reference to the current token in line
			n = 0;
			if ( line.length() == 0 ) {
				System.out.println("Error Reading first line of Input File!!!!");
				//writeHeadingLine();
				//makeDetailLineError();
				//writeDetailLines();
				//e.printStackTrace();
				inFile.closeReader();
		        //outFile.closeWriter();
				System.exit(0);
			}
				else {
					StringTokenizer st = new StringTokenizer(line);
					token = st.nextToken();
					n = Integer.parseInt( token );
					//return n;
			}
		} catch (NumberFormatException e) {
			System.out.println("Error Reading first line of Input File!!!!");
			//writeHeadingLine();
			//makeDetailLineError();
			//writeDetailLines();
			//e.printStackTrace();
			inFile.closeReader();
	        //outFile.closeWriter();
			System.exit(0);
		}
		System.out.println("NumGolfers:" + n);
		return n;
		
    } // end getNumGolfersFromFile()
    
 // Read in golfer (ID, Value, PPM) from each line of InputFile
    public static void getInputGolfers(InputFile inFile) {
    	int numGolfers = getNumGolfersFromFile(inFile);
    	double[] golferArray = new double[3];  
    	String line;
    	String token;		// used to read input
    	StringTokenizer st;
    	
    	try {
    		for(int numLines = 0; numLines < numGolfers; numLines++){
    			line = inFile.readLine();
    			st = new StringTokenizer(line);
    			for(int i = 0; i < 3; i++){
    				token = st.nextToken();
    				golferArray[i] = Double.parseDouble(token);
    			}
    			Golfer golferNext = new Golfer(golferArray[0], golferArray[1], golferArray[2]);
    			TeamManager.addGolfer(golferNext);
    		}
			
			
		} catch (Exception e) {
			System.out.println("Error Populating Vector!!!!");
			//writeHeadingLine();
			//makeDetailLineError();
			//writeDetailLines();
			//e.printStackTrace();
			inFile.closeReader();
	        //outFile.closeWriter();
			System.exit(0);
		}
    	
    } // end getInputGolfers()
    
    // write Heading for Error in Output File
 	public void writeHeadingLine() {
 		 String outFileHeading = String.format("%-25s %-25s %-23s %-18s %-16s", "Stopping reason", "Max num of iterations", "Number of iterations", "Machine epsilon", "X seq tolerance");
 	     outFile.writeln(outFileHeading);
 	 } // end method writeHeaderLine()
 	
 	// Format the line
 	private void makeDetailLineError() {
 		formatter.format(errorMessage);
 	} // end method makeDetailLineError()
 	 
 	// Write detail lines to the Output File
 	private void writeDetailLines() {
 		outFile.writeln(formatter.toString());
 	} // end method writeDetailLines()
}
