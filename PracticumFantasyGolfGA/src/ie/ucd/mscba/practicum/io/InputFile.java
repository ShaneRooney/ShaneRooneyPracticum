/**
 * Created on 11 Oct 2013
 *
 * @author  Sean McGarraghy (sean)
 * @version 1.0
 * 
 * File:      InputFile.java
 * Package:   ie.ucd.mscba.io
 * Project:   Heap
 */

package ie.ucd.mscba.practicum.io;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * General purpose class to support buffered reading a line at a time from 
 * a named file.  It is up to other classes to parse the line, e.g., extract
 * whitespace-separated parameters as tokens.
 *
 * This is not a bullet-proof file reading class by any means: improve if you wish
 * 
 * For example, rather than handle the exceptions locally, as is done here, you 
 * could throw an exception (maybe one from a class you have written yourself) and
 * handle it in the calling routine, as we did in the Exceptions tutorial.
 * 
 * @author  Sean McGarraghy (sean)
 * @version 1.0
 * 
 * Type:      InputFile
 * Package:   ie.ucd.mscba.io
 * Date:Time: 11 Oct 2013 at 23:25:24
 */
public class InputFile
{
    private String inFile;
    private BufferedReader br;

    /**
     * Constructor. Sets the name of the input file, creates a reader object for it.
     * 
     * @param inFile the name of the file with the input data
     */
    public InputFile( String inFile )
    {
        this.inFile = inFile;
        
        try
        {
            br = new BufferedReader( new FileReader( inFile ) );
        }
        catch ( FileNotFoundException e )
        {
            e.printStackTrace();
            System.out.println( "Could not find input file " + inFile + ": program will exit" );
            if ( br != null ) 
            {
                try 
                {
                    br.close();
                    System.out.println( "Reader for file " + inFile + " closed" );
                }
                catch ( IOException o ) 
                {
                    // note the exception: nothing else we can do
                    o.printStackTrace();
                }
            }
            System.exit( 0 ); // throw in the towel
        }
    }
        
    /**
     * Reads a line as a string from the input file the reader has been created for
     * 
     * @param s the string to be read
     */
    public String readLine( )
    {
        String s = new String();
        try
        {
            s =  br.readLine( );
        }
        catch ( IOException o )
        {
            System.out.println( "Error reading from input file: "+ inFile );
            o.printStackTrace( );
            closeReader();
        }
        return s;
    }

    /** 
     * Closes the reader object 
     */
    public void closeReader( )
    {
        if ( br != null ) 
        {
            try 
            {
                br.close();
                System.out.println( "Reader for file " + inFile + " closed" );
            }
            catch ( IOException e ) 
            {
                // note the exception: nothing else we can do
                e.printStackTrace();
                System.out.println( "Error closing reader for input file: "+ inFile );
            }
        }
    }
}
