/**
 * Created on 12 Oct 2013
 *
 * @author  Sean McGarraghy (sean)
 * @version 1.0
 * 
 * File:      OutputFile.java
 * Package:   ie.ucd.mscba.io
 * Project:   Heap
 */
package ie.ucd.mscba.practicum.io;

/**
 * @author  Sean McGarraghy (sean)
 * @version 1.0
 * 
 */
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

/**
 * General purpose class to support buffered writing a line at a time to 
 * a named file.  The output line is a String, produced, e.g., by a 
 * Formatter object.  The writeln() method here adds an end-of-line character.
 *
 * This is not a bullet-proof file writing class by any means: improve if you wish
 * 
 * For example, rather than handle the exceptions locally, as is done here, you 
 * could throw an exception (maybe one from a class you have written yourself) and
 * handle it in the calling routine, as we did in the Exceptions tutorial.
 * 
 * @author  Sean McGarraghy (sean)
 * @version 1.0
 * 
 * Type:      OutputFile
 * Package:   ie.ucd.mscba.io
 * Date:Time: 12 Oct 2013 at 18:09:31
 */
public class OutputFile
{
    private String outFile;
    private BufferedWriter bw;

    /**
     * Constructor. Sets the path of the output, and creates a writer object for it.
     * 
     * @param outFile the name of the file to which output is written
     */
    public OutputFile( String outFile )
    {
        this.outFile = outFile;
        
        try
        {
            bw = new BufferedWriter( new FileWriter( outFile ) );
        }
        catch ( IOException e )
        {
            e.printStackTrace();
            System.out.println( "Error creating file writer for " + outFile );
            if ( bw != null ) 
            {
                try 
                {
                    bw.close();
                    System.out.println( "Writer for file " + outFile + " closed" );
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
     * Writes the specified string to the output file the writer has been 
     * created for
     * 
     * @param s the string to be written
     */
    public void write( String s )
    {
        try
        {
            bw.write( s );
        }
        catch ( IOException o )
        {
            System.out.println( "Error writing to output file: "+ outFile );
            o.printStackTrace( );
            closeWriter();
        }
    }

    /**
     * Writes the specified string to the output file the writer has been 
     * created for, appending a newline \n
     * 
     * @param s the string to be written
     */
    public void writeln( String s )
    {
        write( s + "\n" );
    }

    /** 
     * Closes the writer object 
     */
    public void closeWriter( )
    {
        if ( bw != null ) 
        {
            try
            {
                bw.close();
                System.out.println( "Writer to file " + outFile + " closed" );
            }
            catch ( IOException o )
            {
                // note the exception: nothing else we can do
                System.out.println( "Error closing writer to output file: "+ outFile );
                o.printStackTrace( );
            }
        }
    }
}

