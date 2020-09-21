package jsokoban.base.xml;

import org.jdom.Document;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;
import java.io.FileWriter;
/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave. The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: XMLReaderWriter.java,v $
 * Revision 1.2  2001/10/02 07:20:35  texx
 * implement the parser and writer method
 *
 * Revision 1.1  2001/09/30 17:33:34  texx
 * add
 *
 *
 */


public class XMLReaderWriter
{
  private static final boolean XML_VALIDATION = true;

  public void write(Document doc, String sFilename) throws java.io.IOException
  {
    XMLOutputter xmloutput = new XMLOutputter();
    xmloutput.output(doc,new FileWriter(sFilename));

  }

  public static Document parse(String sFilename) throws org.jdom.JDOMException
  {
      SAXBuilder saxbuilder = new SAXBuilder();
      //saxbuilder.setValidation(XML_VALIDATION);
      return saxbuilder.build(sFilename);
  }


}