package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

import java.util.ArrayList;

public class NotesDataDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "notes_data";

    // Columns
    public static final String COLUMN_NAME_OWNER_ID = "owner_id";
    public static final String COLUMN_NAME_NOTE_TYPE = "note_type"; // robot, team, pit, etc.
    public static final String COLUMN_NAME_NOTE_TEXT = "note_text";
    public static final String COLUMN_NAME_READY_TO_EXPORT = "ready_to_export";

    public static String[] allColumns = {
            _ID,
            COLUMN_NAME_OWNER_ID,
            COLUMN_NAME_NOTE_TYPE,
            COLUMN_NAME_NOTE_TEXT,
            COLUMN_NAME_READY_TO_EXPORT
    };

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
    public NotesDataDBAdapter(Context ctx) {
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
    public NotesDataDBAdapter openForWrite() throws SQLException {
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
    public NotesDataDBAdapter openForRead() throws SQLException {
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getReadableDatabase();
        return this;
    }

    /**
     * close return type: void
     */
    public void close() {
        if(this.mDb != null && this.mDb.isOpen()) {
            this.mDbHelper.close();
        }
        this.mDb = null;
    }

    public boolean dbIsClosed() {
        if(this.mDb == null) {
            return true;
        } else {
            return !this.mDb.isOpen();
        }
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * 
     * @param note_type id
     * @param note_text id
     * @return rowId or -1 if failed
     */
    public long createNotesDataEntry(long owner_id, String note_type, String note_text){
        ContentValues initialValues = new ContentValues();
        initialValues.put(COLUMN_NAME_OWNER_ID, owner_id);
        initialValues.put(COLUMN_NAME_NOTE_TYPE, note_type);
        initialValues.put(COLUMN_NAME_NOTE_TEXT, note_text);
        initialValues.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
        long id = this.openForWrite().mDb.insert(TABLE_NAME, null, initialValues);
        if(!this.dbIsClosed()) this.close();
        return id;
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteNotesDataEntry(long rowId) {
        boolean retVal = this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllNotesDataEntries() {
        return this.openForRead().mDb.query(TABLE_NAME, allColumns, null, null, null, null, null);
    }

    /**
     * Return a Cursor over the list of all entries in the database
     *
     * @return Cursor over all Match Data entries
     */
    public ArrayList<Long> getAllNotesDataEntriesForOwner(long owner_id, String owner_type) {
        ArrayList<Long> noteIDs = new ArrayList<Long>();
        String WHERE = COLUMN_NAME_OWNER_ID + "=" + String.valueOf(owner_id);
        WHERE += " AND " + COLUMN_NAME_NOTE_TYPE + "='" + owner_type + "'";

        Cursor c = this.openForRead().mDb.query(TABLE_NAME, allColumns, WHERE, null, null, null, null);

        while(c.moveToNext()) {
            noteIDs.add(c.getLong(c.getColumnIndex(_ID)));
        }

        return noteIDs;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public String getNotesDataEntry(long rowId) throws SQLException {
        String note = "";
        String WHERE = _ID + "=" + rowId;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns, WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
            note = mCursor.getString(mCursor.getColumnIndex(COLUMN_NAME_NOTE_TEXT));
        }
        if(!mCursor.isClosed()) mCursor.close();
        if(!this.dbIsClosed()) this.close();
        return note;
    }

    /**
     * Update the entry.
     * 
     * @param rowId
     * @param note_type
     * @param note_text
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateNotesDataEntry(int rowId, long owner_id, String note_type, String note_text, Boolean export){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_OWNER_ID, owner_id);
        args.put(COLUMN_NAME_NOTE_TYPE, note_type);
        args.put(COLUMN_NAME_NOTE_TEXT, note_text);
        args.put(COLUMN_NAME_READY_TO_EXPORT, String.valueOf(export));
        boolean retVal = this.openForRead().mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0;
        if(!this.dbIsClosed()) this.close();
        return  retVal;
    }
    public boolean setDataEntryExported(long rowId) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.FALSE.toString());
        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    public Cursor getAllEntriesToExport() {
        String WHERE = COLUMN_NAME_READY_TO_EXPORT + "=" + Boolean.TRUE.toString();
        return this.openForRead().mDb.query(TABLE_NAME, allColumns, WHERE, null, null, null, null);
    }
}
