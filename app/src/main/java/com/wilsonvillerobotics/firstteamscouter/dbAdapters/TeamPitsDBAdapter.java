package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

public class TeamPitsDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "team_pits";
    public static final String COLUMN_NAME_TEAM_ID = "team_id";
    public static final String COLUMN_NAME_PIT_ID = "pit_id";

    private DatabaseHelper mDbHelper;
    private SQLiteDatabase mDb;

    private final Context mCtx;

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
    public TeamPitsDBAdapter(Context ctx) {
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
    public TeamPitsDBAdapter openForWrite() throws SQLException {
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getWritableDatabase();
        return this;
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
    public TeamPitsDBAdapter openForRead() throws SQLException {
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getReadableDatabase();
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
     * @param rowId
     * @param team_id
     * @param pit_id
     * @return rowId or -1 if failed
     */
    public long createTeamPit(int rowId, int team_id, int pit_id){
        ContentValues args = new ContentValues();
        args.put(_ID, rowId);
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_PIT_ID, pit_id);
        return this.mDb.insert(TABLE_NAME, null, args);
        /*

         */
    }

    /**
     * Update the entry.
     * 
     * @param rowId
     * @param team_id
     * @param pit_id
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateTeamPit(int rowId, int team_id, int pit_id){
        ContentValues args = new ContentValues();
        args.put(_ID, rowId);
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_PIT_ID, pit_id);
        return this.mDb.update(TABLE_NAME, args, COLUMN_NAME_PIT_ID + "=" + pit_id, null) >0;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllTeamPits() {

        return this.mDb.query(TABLE_NAME, new String[] { _ID,
        		_ID, COLUMN_NAME_TEAM_ID, COLUMN_NAME_PIT_ID
        		}, null, null, null, null, null);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param teamId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getTeamPit(long teamId) throws SQLException {

        Cursor mCursor =

        this.mDb.query(true, TABLE_NAME, new String[] { _ID, COLUMN_NAME_TEAM_ID, COLUMN_NAME_PIT_ID },
                COLUMN_NAME_TEAM_ID + "=" + teamId, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteTeamPit(long rowId) {

        return this.mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }
}
