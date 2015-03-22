package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

public class PictureDataDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME = "picture_data";

    // Columns
    public static final String COLUMN_NAME_OWNER_ID = "owner_id";
    public static final String COLUMN_NAME_PICTURE_TYPE = "picture_type"; // robot, team, pit, etc.
    public static final String COLUMN_NAME_PICTURE_URI = "picture_uri";
    //public static final String COLUMN_NAME_READY_TO_EXPORT = "ready_to_export";

    public static String[] allColumns = {
            _ID,
            COLUMN_NAME_TABLET_ID,
            COLUMN_NAME_OWNER_ID,
            COLUMN_NAME_PICTURE_TYPE,
            COLUMN_NAME_PICTURE_URI,
            COLUMN_NAME_READY_TO_EXPORT
    };

    @Override
    public String[] getAllColumns() {
        return allColumns;
    }

    /**
     * Constructor - takes the context to allow the database to be
     * opened/created
     * 
     * @param ctx
     *            the Context within which to work
     */
    public PictureDataDBAdapter(Context ctx) {
        super(ctx);
        //this.mCtx = ctx;
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
    public PictureDataDBAdapter openForWrite() throws SQLException {
        return (PictureDataDBAdapter)openDBForWrite();
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
    public PictureDataDBAdapter openForRead() throws SQLException {
        return (PictureDataDBAdapter)openDBForRead();
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * 
     * @param picture_type
     * @param picture_uri
     * @return rowId or -1 if failed
     */
    public long createPictureDataEntry(long owner_id, String picture_type, String picture_uri){
        ContentValues initialValues = new ContentValues();
        initialValues.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
        initialValues.put(COLUMN_NAME_OWNER_ID, owner_id);
        initialValues.put(COLUMN_NAME_PICTURE_TYPE, picture_type);
        initialValues.put(COLUMN_NAME_PICTURE_URI, picture_uri);
        initialValues.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
        return this.openForWrite().mDb.insert(TABLE_NAME, null, initialValues);
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    @Override
    public boolean deleteEntry(long rowId) {
        return super.deleteEntry(rowId, TABLE_NAME);
        /*
        return this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
        */
    }

    @Override
    public boolean deleteAllEntries() {
        return super.deleteAllEntries(TABLE_NAME);
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    @Override
    public Cursor getAllEntries() {
        return super.getAllEntries(TABLE_NAME, allColumns);
        //return this.openForRead().mDb.query(TABLE_NAME, allColumns, null, null, null, null, null);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getEntry(long rowId) throws SQLException {
        return super.getEntry(rowId, TABLE_NAME, allColumns);
        /*
        String WHERE = _ID + "=" + rowId;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns, WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
        */
    }

    /**
     * Update the entry.
     * 
     * @param rowId
     * @param picture_id
     * @param picture_type
     * @param picture_uri
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updatePictureDataEntry(int rowId, long owner_id, int picture_id, String picture_type, String picture_uri, Boolean export){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_OWNER_ID, owner_id);
        args.put(COLUMN_NAME_PICTURE_TYPE, picture_type);
        args.put(COLUMN_NAME_PICTURE_URI, picture_uri);
        args.put(COLUMN_NAME_READY_TO_EXPORT, String.valueOf(export));
        boolean retVal = this.mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0;
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
}
