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

public class CompetitionDataDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "competition_data";
    public static final String COLUMN_NAME_COMPETITION_ID = "competition_id";
    public static final String COLUMN_NAME_COMPETITION_NAME = "competition_name";
    public static final String COLUMN_NAME_COMPETITION_LOCATION = "competition_location";

    /*
    public String[] Columns = {
    		COLUMN_NAME_COMPETITION_ID,
    		COLUMN_NAME_COMPETITION_NAME,
    		COLUMN_NAME_COMPETITION_LOCATION
    };
    */
    
    private DatabaseHelper mDbHelper;
    private SQLiteDatabase mDb;

    private final Context mCtx;

    private static class DatabaseHelper extends SQLiteOpenHelper {

        DatabaseHelper(Context context) {
            super(context, DBAdapter.DATABASE_NAME, null, DBAdapter.DATABASE_VERSION);
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
        this.mCtx = ctx;
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
    public CompetitionDataDBAdapter open() throws SQLException {
        this.mDbHelper = new DatabaseHelper(this.mCtx);
        this.mDb = this.mDbHelper.getWritableDatabase();
        return this;
    }

    /**
     * close return type: void
     */
    public void close() {
        this.mDbHelper.close();
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
        initialValues.put(COLUMN_NAME_COMPETITION_ID, id);
        initialValues.put(COLUMN_NAME_COMPETITION_NAME, name);
        initialValues.put(COLUMN_NAME_COMPETITION_LOCATION, location);
        return this.mDb.insert(TABLE_NAME, null, initialValues);
    }

    /**
     * Delete the Competition datum with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteCompetitionDataEntry(long rowId) {

        return this.mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }

    /**
     * Return a Cursor over the list of all cars in the database
     * 
     * @return Cursor over all Competition Data entries
     */
    public Cursor getAllCompetitionDataEntries() {

        return this.mDb.query(TABLE_NAME, new String[] { _ID,
        		COLUMN_NAME_COMPETITION_ID, COLUMN_NAME_COMPETITION_NAME, COLUMN_NAME_COMPETITION_LOCATION }, null, null, null, null, null);
    }

    /**
     * Return a Cursor positioned at the Competition Data Entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching Competition Data Entry, if found
     * @throws SQLException if Competition Data Entry could not be found/retrieved
     */
    public Cursor getCompetitionDataEntry(long rowId) throws SQLException {

        Cursor mCursor =

        this.mDb.query(true, TABLE_NAME, new String[] { _ID, COLUMN_NAME_COMPETITION_ID,
        		COLUMN_NAME_COMPETITION_NAME, COLUMN_NAME_COMPETITION_LOCATION}, _ID + "=" + rowId, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
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
            String location){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_COMPETITION_ID, id);
        args.put(COLUMN_NAME_COMPETITION_NAME, name);
        args.put(COLUMN_NAME_COMPETITION_LOCATION, location);

        return this.mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0; 
    }

}
