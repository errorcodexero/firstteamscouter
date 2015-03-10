package com.wilsonvillerobotics.firstteamscouter.utilities;

import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Environment;
import android.util.Log;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.CompetitionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.DBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.NotesDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PictureDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionsDBAdapter;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

/**
 * Android DataExporter that allows the passed in SQLiteDatabase
 * to be exported to external storage (SD card) in an XML format.
 *
 * To backup a SQLite database you need only copy the database file itself
 * (on Android /data/data/APP_PACKAGE/databases/DB_NAME.db) -- you *don't* need this
 * export to XML step.
 *
 * XML export is useful so that the data can be more easily transformed into
 * other formats and imported/exported with other tools (not for backup per se).
 *
 * The kernel of inspiration for this came from:
 * http://mgmblog.com/2009/02/06/export-an-android-sqlite-db-to-an-xml-file-on-the-sd-card/.
 * (Though I have made many changes/updates here, I did initially start from that article.)
 *
 * @author ccollins
 *
 * https://code.google.com/p/and-bookworm/source/browse/trunk/src/com/totsp/bookworm/data/DataXmlExporter.java?r=407
 */
public class DataXmlExporter {

    private static final String DATASUBDIRECTORY = "files";
    private final String LOG_TAG = "FTS_XmlExporter";

    private final SQLiteDatabase db;
    private XmlBuilder xmlBuilder;

    public DataXmlExporter(final SQLiteDatabase db) {
        this.db = db;
    }

    public boolean export(final String dbName, final String exportFileNamePrefix) throws IOException {
        Log.i(LOG_TAG, "exporting database - " + dbName + " exportFileNamePrefix=" + exportFileNamePrefix);

        xmlBuilder = new XmlBuilder();
        xmlBuilder.start(dbName);

        // get the tables
        //String sql = "select * from sqlite_master";
        /*String sql = "select * from sqlite_temp_master";
        Cursor c = db.rawQuery(sql, new String[0]);
        if (c.moveToFirst()) {
            do {
                String tableName = c.getString(c.getColumnIndex("name"));

                // skip metadata, sequence, and uidx (unique indexes)
                if (!tableName.equals("android_metadata") && !tableName.equals("sqlite_sequence")
                        && !tableName.startsWith("uidx")) {
                    exportTable(tableName);
                }
            } while (c.moveToNext());
        }*/
        String table_names[] = {
                CompetitionDataDBAdapter.TABLE_NAME,
                MatchDataDBAdapter.TABLE_NAME,
                NotesDataDBAdapter.TABLE_NAME,
                PictureDataDBAdapter.TABLE_NAME,
                PitDataDBAdapter.TABLE_NAME,
                RobotDataDBAdapter.TABLE_NAME,
                TeamDataDBAdapter.TABLE_NAME,
                TeamMatchDBAdapter.TABLE_NAME,
                TeamMatchTransactionDataDBAdapter.TABLE_NAME,
                TeamMatchTransactionsDBAdapter.TABLE_NAME
        };

        for(String tableName : table_names) {
            try {
                exportTable(tableName);
            } catch(Exception e) {
                e.printStackTrace();
            }
        }
        String xmlString = xmlBuilder.end();
        //if(!c.isClosed()) c.close();
        String xmlFileName = exportFileNamePrefix + ".xml";
        boolean exported = writeToFile(xmlString, xmlFileName);
        Log.i(LOG_TAG, "exporting database complete");
        return exported;
    }

    private void exportTable(final String tableName) throws IOException {
        xmlBuilder.openTable(tableName);
        String sql = "select * from " + tableName;
        Cursor c = db.rawQuery(sql, new String[0]);
        if (c.moveToFirst()) {
            int cols = c.getColumnCount();
            do {
                xmlBuilder.openRow();
                for (int i = 0; i < cols; i++) {
                    xmlBuilder.addColumn(c.getColumnName(i), c.getString(i));
                }
                xmlBuilder.closeRow();
            } while (c.moveToNext());
        }
        c.close();
        xmlBuilder.closeTable();
    }

    private boolean writeToFile(final String xmlString, final String exportFileName) throws IOException {
        boolean exportSuccess = true;
        File dir = new File(Environment.getExternalStorageDirectory(), DataXmlExporter.DATASUBDIRECTORY);
        if (!dir.exists()) {
            dir.mkdirs();
        }
        File file = new File(dir, exportFileName);
        file.createNewFile();

        ByteBuffer buff = ByteBuffer.wrap(xmlString.getBytes());
        FileChannel channel = new FileOutputStream(file).getChannel();
        try {
            channel.write(buff);
        } catch (Exception e) {
            e.printStackTrace();
            exportSuccess = false;
        } finally {
            if (channel != null) {
                channel.close();
            }
        }

      /*
      FileWriter fw = new FileWriter(file);
      if (fw != null) {
         try {
            fw.write(xmlString);
            fw.flush();
         } finally {
            fw.close();
         }
      }
      */

        return exportSuccess;
    }

    /**
     * XmlBuilder is used to write XML tags (open and close, and a few attributes)
     * to a StringBuilder. Here we have nothing to do with IO or SQL, just a fancy StringBuilder.
     *
     * @author ccollins
     *
     */
    static class XmlBuilder {
        private static final String OPEN_XML_STANZA = "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
        private static final String CLOSE_WITH_TICK = "'>";
        private static final String DB_OPEN = "<database name='";
        private static final String DB_CLOSE = "</database>";
        private static final String TABLE_OPEN = "<table name='";
        private static final String TABLE_CLOSE = "</table>";
        private static final String ROW_OPEN = "<row>";
        private static final String ROW_CLOSE = "</row>";
        private static final String COL_OPEN = "<col name='";
        private static final String COL_CLOSE = "</col>";

        private final StringBuilder sb;

        public XmlBuilder() throws IOException {
            sb = new StringBuilder();
        }

        void start(final String dbName) {
            sb.append(XmlBuilder.OPEN_XML_STANZA);
            sb.append(XmlBuilder.DB_OPEN + dbName + XmlBuilder.CLOSE_WITH_TICK);
        }

        String end() throws IOException {
            sb.append(XmlBuilder.DB_CLOSE);
            return sb.toString();
        }

        void openTable(final String tableName) {
            sb.append(XmlBuilder.TABLE_OPEN + tableName + XmlBuilder.CLOSE_WITH_TICK);
        }

        void closeTable() {
            sb.append(XmlBuilder.TABLE_CLOSE);
        }

        void openRow() {
            sb.append(XmlBuilder.ROW_OPEN);
        }

        void closeRow() {
            sb.append(XmlBuilder.ROW_CLOSE);
        }

        void addColumn(final String name, final String val) throws IOException {
            sb.append(XmlBuilder.COL_OPEN + name + XmlBuilder.CLOSE_WITH_TICK + val + XmlBuilder.COL_CLOSE);
        }
    }

}