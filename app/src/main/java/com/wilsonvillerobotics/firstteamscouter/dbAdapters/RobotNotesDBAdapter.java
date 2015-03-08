package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

import java.util.ArrayList;

public class RobotNotesDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "robot_notes";
    public static final String COLUMN_NAME_ROBOT_ID = "robot_id";
    public static final String COLUMN_NAME_NOTE_ID = "note_id";

    private DatabaseHelper mDbHelper;
    private SQLiteDatabase mDb;

    private final Context mCtx;

    private static class DatabaseHelper extends SQLiteOpenHelper {

        private static DatabaseHelper mInstance = null;

        private DatabaseHelper(Context context) {
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
    public RobotNotesDBAdapter(Context ctx) {
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
    public RobotNotesDBAdapter openForWrite() throws SQLException {
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
    public RobotNotesDBAdapter openForRead() throws SQLException {
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
     * @param robot_id
     * @param note_id
     * @return rowId or -1 if failed
     */
    public long createRobotNote(long robot_id, long note_id){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_ROBOT_ID, robot_id);
        args.put(COLUMN_NAME_NOTE_ID, note_id);
        return this.mDb.insert(TABLE_NAME, null, args);
    }

    /**
     * Update the entry.
     * 
     * @param rowId
     * @param robot_id
     * @param note_id
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateRobotNote(int rowId, long robot_id, long note_id){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_ROBOT_ID, robot_id);
        args.put(COLUMN_NAME_NOTE_ID, note_id);
        return this.mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0; 
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteRobotNote(long rowId) {

        return this.mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllRobotNotes() {

        return this.mDb.query(TABLE_NAME, new String[] {
                _ID, COLUMN_NAME_ROBOT_ID, COLUMN_NAME_NOTE_ID
        		}, null, null, null, null, null);
    }

    /**
     * Return a Cursor over the list of all entries for a robot
     *
     * @return Cursor over all Robot Note ID entries
     */
    public ArrayList<Long> getAllRobotNoteIdsForRobotId(long robotId) {
        String WHERE = COLUMN_NAME_ROBOT_ID + "=" + robotId;
        Cursor c = this.mDb.query(TABLE_NAME, new String[] {
                _ID, COLUMN_NAME_ROBOT_ID, COLUMN_NAME_NOTE_ID
        }, WHERE, null, null, null, null);
        ArrayList<Long> noteIdList = new ArrayList<Long>();
        while(c.moveToNext()) {
            noteIdList.add(c.getLong(c.getColumnIndex(COLUMN_NAME_NOTE_ID)));
        }
        return noteIdList;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getRobotNote(long rowId) throws SQLException {

        Cursor mCursor =

        this.mDb.query(true, TABLE_NAME, new String[] {
                _ID, COLUMN_NAME_ROBOT_ID, COLUMN_NAME_NOTE_ID
        		}, _ID + "=" + rowId, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }
}
