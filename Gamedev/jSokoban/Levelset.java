package jsokoban.base;

import jsokoban.base.xml.XMLReaderWriter;
import org.jdom.Document;
import org.jdom.Element;
import java.util.Vector;
import java.util.List;
import java.util.Iterator;
/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: Levelset.java,v $
 * Revision 1.6  2001/10/04 16:31:48  texx
 * fix bug in the load method
 *
 * Revision 1.5  2001/10/03 19:20:57  texx
 * implement a first version of the loadLevelSet method
 *
 * Revision 1.4  2001/09/30 14:04:00  texx
 * correct the return type of nextLevel()
 *
 * Revision 1.3  2001/09/28 18:48:21  texx
 * update the class ( the interface changed )
 *
 * Revision 1.2  2001/09/28 18:42:45  texx
 * Add cvs keywords
 *
 */

public class Levelset implements JSokobanLevelSet
{
  private final String XML_ROOT_ELEMENT = "jsokoban";
  private final String XML_MAIN_ELEMENT = "levelset";
  private final String XML_LEVEL_ATTRIBUTE_FILE = "file";

  private int iLevelAt = 0;
  private String m_sFilename = "";
  private Vector m_vLevel  = null;

  public Levelset()
  {
  }

  /**
   * return the next level
   */
  public JSokobanLevel nextLevel()
  {
    Level lReturn = (Level)m_vLevel.elementAt(iLevelAt);
    iLevelAt++;
    return lReturn;
  }

  /**
   * loads a levelset file
   * return -1 if something go wrong
   * return -2 when parsing go wrong
   * return 1 when success
   */
  public int loadLevelSet(String sFilename)
  {
    Document doc;
    try
    {
      doc = XMLReaderWriter.parse(sFilename);
    }
    catch (Exception ex)
    {
      return -2;
    }

    if (!doc.getRootElement().getName().equals(XML_ROOT_ELEMENT))
    {
      return -1;
    }
    if (doc.getRootElement().getChild(XML_MAIN_ELEMENT) == null )
    {
      return -1;
    }
    List list = doc.getRootElement().getChild(XML_MAIN_ELEMENT).getChildren();
    Iterator iterator = list.iterator();
    m_vLevel = new Vector();
    Level lTemp;
    while (iterator.hasNext())
    {
      Element el = (Element)iterator.next();
      lTemp = new Level();

      String sFile = el.getAttributeValue(XML_LEVEL_ATTRIBUTE_FILE);
      String sName = el.getText();

      if (sFile == null || sName == null)
      {
        return -1;
      }
      lTemp.setFile(sFile);
      lTemp.setName(sName);
      m_vLevel.addElement(lTemp);
    }

    return 1;
  }

  /**
   * return true if the levelset has more level
   */
  public boolean hasMoreLevel()
  {
    if (m_vLevel != null && iLevelAt < m_vLevel.size())
    {
      return true;
    }

    return false;
  }
}
