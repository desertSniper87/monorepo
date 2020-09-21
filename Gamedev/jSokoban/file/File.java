package jsokoban.base.file;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Vector;
import jsokoban.base.exception.FileException;
/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: File.java,v $
 * Revision 1.2  2001/09/28 18:42:45  texx
 * Add cvs keywords
 *
 */

public class File
{
  private String m_sFilename = "";


  public File( String sFilename ) throws FileException
  {
    m_sFilename = sFilename;
    this.readFile();
  }

  public Vector readFile() throws FileException
  {
    Vector vReturn = new Vector();
    String sLine = "";
    try
    {
      BufferedReader bufferedReader = new BufferedReader(new FileReader(m_sFilename));
      while ( (sLine = bufferedReader.readLine()) != null )
      {
        vReturn.add(sLine);
      }
      return vReturn;
    }
    catch ( Exception e )
    {
      throw new FileException("Error while reading file");
    }


  }
}