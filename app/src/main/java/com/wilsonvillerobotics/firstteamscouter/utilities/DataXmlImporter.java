package com.wilsonvillerobotics.firstteamscouter.utilities;

import android.content.Context;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.DBAdapter;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlPullParserFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

/**
 * Found by Tom on 3/9/2015.
 *
 * http://stackoverflow.com/questions/21114140/how-to-import-data-from-xml-file-to-sqlite-database
 * MySQLPullParser spp = new MySQLPullParser();
 * db.execSQL( spp.parseXML(mContext) );
 */
public class DataXmlImporter {

    private String currTag = null;
    private boolean firstTag = true;
    private String currName = "";
    private String fileName;

    public DataXmlImporter(String filename) {
        this.fileName = filename;
    }

    public String parseXML(Context ctx, String tableName, String firstCol, String lastCol) {
        StringBuilder sb = new StringBuilder(500);  // give sufficient length to start with
        String xmlText;
        try {
            sb.append("INSERT INTO " + tableName + " VALUES "); //(");

            XmlPullParserFactory xppFactory = XmlPullParserFactory.newInstance();
            xppFactory.setNamespaceAware(true);
            XmlPullParser xpp = xppFactory.newPullParser();

            //URL yourXmlPath = new URL(fileName);
            File f = new File(fileName);
            FileInputStream fi = new FileInputStream(f);
            //InputStream is = yourXmlPath.openConnection().getInputStream();

            xpp.setInput(fi,null);

            int e = xpp.getEventType();
            while (e != XmlPullParser.END_DOCUMENT)
            {
                if(e == XmlPullParser.START_TAG) {
                    currTag = xpp.getName();
                }
                else if (e == XmlPullParser.END_TAG) {
                    currTag = null;
                }
                else if (e == XmlPullParser.TEXT) {
                    xmlText = xpp.getText();
                    if(currTag != null) {
                        if (currTag.equals(firstCol)) {    // first table column
                            if (firstTag) { // no single quotes for the _id field
                                sb.append("(" + xmlText); // for first row insert
                                firstTag = false;
                            } else {
                                sb.append(",(" + xmlText);
                            }
                        } else if (currTag.equals(lastCol)) {
                            sb.append(", '" + xmlText + "')");  // last table column should have a closing paren ")"
                        } else if(xmlText != null && !xmlText.startsWith("\n")) {
                            sb.append(", '" + xmlText + "'");
                        }
                    }
                }
                e = xpp.next();
            }
            //sb.append(")");

        }   catch (XmlPullParserException e) {
            e.printStackTrace();
        } catch (IOException e1) {
            e1.printStackTrace();
        }

        return sb.toString();
    }
}