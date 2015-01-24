package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

public class PictureDataDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "picture_data";
    public static final String COLUMN_NAME_PICTURE_ID = "picture_id";
    public static final String COLUMN_NAME_PICTURE_TYPE = "picture_type"; // robot, team, pit, etc.
    public static final String COLUMN_NAME_PICTURE_URI = "picture_uri";

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
    public PictureDataDBAdapter(Context ctx) {
        this.mCtx = ctx;
    }

    /**
     * Open the FirstTeamScouter database. If it cannot be opened, try to create a new
     * instance of the database. If it cannot be created, throw an exception to
     * signal the failure
     * 
     * @return this (self reference, allowing this to be chained in an
     *         initialization call)
     * @throws SQLException
     *             if the database could be neither opened or created
     */
    public PictureDataDBAdapter open() throws SQLException {
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
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * 
     * @param match note id
     * @param match id
     * @param note id
     * @return rowId or -1 if failed
     */
    public long createPictureDataEntry(int picture_id, String picture_type, String picture_uri){
        ContentValues initialValues = new ContentValues();
        initialValues.put(COLUMN_NAME_PICTURE_ID, picture_id);
        initialValues.put(COLUMN_NAME_PICTURE_TYPE, picture_type);
        initialValues.put(COLUMN_NAME_PICTURE_URI, picture_uri);
        return this.mDb.insert(TABLE_NAME, null, initialValues);
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deletePictureDataEntry(long rowId) {

        return this.mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllPictureDataEntries() {

        return this.mDb.query(TABLE_NAME, new String[] { _ID,
        		COLUMN_NAME_PICTURE_ID, COLUMN_NAME_PICTURE_TYPE, COLUMN_NAME_PICTURE_URI
        		}, null, null, null, null, null);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getPictureDataEntry(long rowId) throws SQLException {

        Cursor mCursor =

        this.mDb.query(true, TABLE_NAME, new String[] { _ID, 
        		COLUMN_NAME_PICTURE_ID, COLUMN_NAME_PICTURE_TYPE, COLUMN_NAME_PICTURE_URI 
        		}, _ID + "=" + rowId, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    /**
     * Update the entry.
     * 
     * @param rowId
     * @param match_note_id
     * @param match_id
     * @param note_id
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updatePictureDataEntry(int rowId, int picture_id, String picture_type, String picture_uri){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_PICTURE_ID, picture_id);
        args.put(COLUMN_NAME_PICTURE_TYPE, picture_type);
        args.put(COLUMN_NAME_PICTURE_URI, picture_uri);
        return this.mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0; 
    }

}
