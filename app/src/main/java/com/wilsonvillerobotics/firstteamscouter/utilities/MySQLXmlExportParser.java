package com.wilsonvillerobotics.firstteamscouter.utilities;

import android.content.Context;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlPullParserFactory;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

/**
 * Created by Tom on 3/9/2015.
 *
 * http://stackoverflow.com/questions/21114140/how-to-import-data-from-xml-file-to-sqlite-database
 * MySQLPullParser spp = new MySQLPullParser();
 * db.execSQL( spp.parseXML(mContext) );
 */
public class MySQLXmlExportParser {

    private String currTag = null;
    private boolean firstTag = true;
    private String fileName;

    public MySQLXmlExportParser(String filename) {
        this.fileName = filename;
    }

    public String parseXML(Context ctx, String tableName) {
        StringBuilder sb = new StringBuilder(500);  // give sufficient length to start with
        try {
            sb.append("INSERT INTO " + tableName + " VALUES (");

            XmlPullParserFactory xppFactory = XmlPullParserFactory.newInstance();
            xppFactory.setNamespaceAware(true);
            XmlPullParser xpp = xppFactory.newPullParser();

            URL yourXmlPath = new URL(fileName);
            InputStream is = yourXmlPath.openConnection().getInputStream();

            xpp.setInput(is,null);

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
                    if ( currTag.equals("state") ) {    // first table column
                        if (firstTag)
                            sb.append( xmlText + "(" ); // for first row insert
                        else
                            sb.append( xmlText + ",(" );
                    }

                    else if ( currTagType.equals("district") ){
                        sb.append( "'" + xmlText + "')" );  // last table column should have a closing paran ")"
                    }
                }
                e = xpp.next();
            }

        }   catch (XmlPullParserException e) {
            e.printStackTrace();
        } catch (IOException e1) {
            e1.printStackTrace();
        }

        return sb.toString();
    }
}