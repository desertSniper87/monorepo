package jsokoban.base.exception;

/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: FileException.java,v $
 * Revision 1.2  2001/09/28 18:42:45  texx
 * Add cvs keywords
 *
 */

public class FileException extends Exception
{

  public FileException(String sDescr)
  {
    super(sDescr);
  }
}