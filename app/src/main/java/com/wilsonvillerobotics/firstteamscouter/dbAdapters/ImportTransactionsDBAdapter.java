package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.ArrayList;

public class ImportTransactionsDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME = "import_transactions";

    // Columns
    public static final String COLUMN_NAME_IMPORTED_FILE_NAME = "imported_file_name";

    public static String[] allColumns = new String[]{
    		_ID,
            COLUMN_NAME_TABLET_ID,
            COLUMN_NAME_IMPORTED_FILE_NAME
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
    public ImportTransactionsDBAdapter(Context ctx) {
        super((ctx));
        //this.mCtx = ctx;
    }

    /**
     * Open the FirstTeamScouter database. If it cannot be opened, try to create a new
     * instance of the database. If it cannot be created, throw an exception to
     * signal the failure
     *
     * @return this (self reference, allowing this to be chained in an
     *         initialization call)
     * @throws android.database.SQLException
     *             if the database could be neither opened or created
     */
    public ImportTransactionsDBAdapter openForWrite() throws SQLException {
    	return (ImportTransactionsDBAdapter)openDBForWrite();
    }

    /**
     * Open the FirstTeamScouter database. If it cannot be opened, try to create a new
     * instance of the database. If it cannot be created, throw an exception to
     * signal the failure
     *
     * @return this (self reference, allowing this to be chained in an
     *         initialization call)
     * @throws android.database.SQLException
     *             if the database could be neither opened or created
     */
    public ImportTransactionsDBAdapter openForRead() throws SQLException {
        return (ImportTransactionsDBAdapter)openDBForRead();
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * @param fileName
     * @return rowId or -1 if failed
     */
    public long addImportedFile(String fileName) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_IMPORTED_FILE_NAME, fileName);
        long id = this.openForWrite().mDb.insert(TABLE_NAME, null, args);
        return id;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     *
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllEntries() {
        return this.openForRead().mDb.query(TABLE_NAME, this.allColumns, null, null, null, null, _ID);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowID
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    @Override
    public Cursor getEntry(long rowID) throws SQLException {
        FTSUtilities.printToConsole("MatchDataDBAdapter::getEntry : matchID: " + rowID + "\n");
        String WHERE = _ID + "=" + rowID;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns, WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    @Override
    /**
     * This table doesn't get exported
     */
    public boolean setEntryExported(long rowId) {
        return false;
    }

    @Override
    /**
     * This table doesn't get exported
     */
    public Cursor getAllEntriesToExport() {
        return null;
    }

    public boolean fileHasNotBeenImported(String fileName) {
        boolean fileExists = false;
        String WHERE = COLUMN_NAME_IMPORTED_FILE_NAME + " = '" + fileName + "' ";
        Cursor c = this.openForRead().mDb.query(TABLE_NAME, this.allColumns, WHERE, null, null, null, null );
        fileExists = (c != null && c.getCount() > 0);
        if(!c.isClosed()) c.close();
        if(!this.dbIsClosed()) this.close();
        return !fileExists;
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteEntry(long rowId) {
        return this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }

    public boolean deleteAllEntries() {
        return this.openForWrite().mDb.delete(TABLE_NAME, null, null) > 0;
    }
}

