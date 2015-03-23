package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

/*
 * 
 * THIS IS AN EXAMPLE TO USE - 
 * 
 * PATTERN DBADAPTERS FOR EACH TABLE AFTER THIS
 * 
 * THEN UPDATE DBADAPTER.java
 * 
 */
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.ArrayList;

public class CompetitionDataDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME = "competition_data";

    // Columns
    public static final String COLUMN_NAME_COMPETITION_NAME = "competition_name";
    public static final String COLUMN_NAME_COMPETITION_LOCATION = "competition_location";

    public static String[] allColumns = {
    		_ID,
            COLUMN_NAME_TABLET_ID,
    		COLUMN_NAME_COMPETITION_NAME,
    		COLUMN_NAME_COMPETITION_LOCATION,
            COLUMN_NAME_READY_TO_EXPORT
    };

    public String[] getAllColumns() {
        return allColumns;
    }

    private DatabaseHelper mDbHelper;
    private SQLiteDatabase mDb;

    private Context mCtx;

    public static boolean restoreTableData(ArrayList<Object> data) {
        return false;
    }

    private static class DatabaseHelper extends SQLiteOpenHelper {

        private static DatabaseHelper mInstance = null;

        DatabaseHelper(Context context) {
            super(context, DBAdapter.DATABASE_NAME, null, DBAdapter.DATABASE_VERSION);
        }

        public static DatabaseHelper getInstance(Context ctx) {

            // Use the application context, which will ensure that you
            // don't accidentally leak an Activity's context.
            // See this article for more information: http://bit.ly/6LRzfx
            if (mInstance == null) {
                mInstance = new DatabaseHelper(ctx.getApplicationContext());
            }
            return mInstance;
        }

        @Override
        public void onCreate(SQLiteDatabase db) {
        }

        @Override
        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        }
        
        @Override
    	public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
            onUpgrade(db, oldVersion, newVersion);
        }
    }

    /**
     * Constructor - takes the context to allow the database to be
     * opened/created
     * 
     * @param ctx
     *            the Context within which to work
     */
    public CompetitionDataDBAdapter(Context ctx) {
        super(ctx);
        //this.mCtx = ctx;
    }

    /**
     * Open the CompetitionData database. If it cannot be opened, try to create a new
     * instance of the database. If it cannot be created, throw an exception to
     * signal the failure
     * 
     * @return this (self reference, allowing this to be chained in an
     *         initialization call)
     * @throws SQLException
     *             if the database could be neither opened or created
     */
    public CompetitionDataDBAdapter openForWrite() throws SQLException {
        return (CompetitionDataDBAdapter)openDBForWrite();
    }

    /**
     * Open the CompetitionData database. If it cannot be opened, try to create a new
     * instance of the database. If it cannot be created, throw an exception to
     * signal the failure
     *
     * @return this (self reference, allowing this to be chained in an
     *         initialization call)
     * @throws SQLException
     *             if the database could be neither opened or created
     */
    public CompetitionDataDBAdapter openForRead() throws SQLException {
        return (CompetitionDataDBAdapter)openDBForRead();
    }

    /**
     * Create a new Competition entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * 
     * @param id
     * @param name
     * @param location
     * @return rowId or -1 if failed
     */
    public long createCompetitionDataEntry(int id, String name, String location){
        ContentValues initialValues = new ContentValues();
        initialValues.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
        initialValues.put(COLUMN_NAME_COMPETITION_NAME, name);
        initialValues.put(COLUMN_NAME_COMPETITION_LOCATION, location);
        initialValues.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
        long retVal = this.openForWrite().mDb.insert(TABLE_NAME, null, initialValues);
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    /**
     * Delete the Competition datum with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    @Override
    public boolean deleteEntry(long rowId) {
        return super.deleteEntry(rowId, TABLE_NAME);
        /*
        boolean retVal = this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
        */
    }

    /**
     * Return a Cursor over the list of all cars in the database
     * 
     * @return Cursor over all Competition Data entries
     */
    @Override
    public Cursor getAllEntries() {
        return super.getAllEntries(TABLE_NAME, allColumns);
        //return this.openForRead().mDb.query(TABLE_NAME, allColumns, null, null, null, null, null);
    }

    /**
     * Return a Cursor positioned at the Competition Data Entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching Competition Data Entry, if found
     * @throws SQLException if Competition Data Entry could not be found/retrieved
     */
    @Override
    public Cursor getEntry(long rowId) throws SQLException {
        return super.getEntry(rowId, TABLE_NAME, allColumns);
        /*
        String WHERE = _ID + "=" + rowId;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns, WHERE, null, null, null, null, null);
        return mCursor;
        */
    }

    /**
     * Update the Competition Data Entry.
     * 
     * @param rowId
     * @param id
     * @param name
     * @param location
     * @return true if the Competition Data Entry was successfully updated, false otherwise
     */
    public boolean updateCompetitionDataEntry(long rowId, int id, String name,
            String location, Boolean export){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_COMPETITION_NAME, name);
        args.put(COLUMN_NAME_COMPETITION_LOCATION, location);
        args.put(COLUMN_NAME_READY_TO_EXPORT, String.valueOf(export));
        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    @Override
    public boolean setEntryExported(long rowId) {
        return super.setEntryExported(rowId, TABLE_NAME);
        /*
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.FALSE.toString());
        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
        */
    }

    @Override
    public Cursor getAllEntriesToExport() {
        return super.getAllEntriesToExport(TABLE_NAME, allColumns);
        /*
        String WHERE = COLUMN_NAME_READY_TO_EXPORT + "=" + Boolean.TRUE.toString();
        return this.openForRead().mDb.query(TABLE_NAME, allColumns, WHERE, null, null, null, null);
        */
    }

    @Override
    public boolean deleteAllEntries() {
        return super.deleteAllEntries(TABLE_NAME);
        //return this.openForWrite().mDb.delete(TABLE_NAME, null, null) > 0;
    }
}
