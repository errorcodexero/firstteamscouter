package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

/**
 * Created by Tom on 3/21/2015.
 */
public class FTSDBAdapter {
    protected DatabaseHelper mDbHelper;
    protected SQLiteDatabase mDb;

    protected final Context mCtx;

    public static final String COLUMN_NAME_TABLET_ID = "tablet_id";
    public static final String COLUMN_NAME_READY_TO_EXPORT = "ready_to_export";

    protected static class DatabaseHelper extends SQLiteOpenHelper {

        protected static DatabaseHelper mInstance = null;

        protected DatabaseHelper(Context context) {
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
    public FTSDBAdapter(Context ctx) {
        this.mCtx = ctx;
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
    public FTSDBAdapter openDBForWrite() throws SQLException {
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
    public FTSDBAdapter openDBForRead() throws SQLException {
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
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param iD
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getEntry(long iD, String tableName, String[] allColumns) throws SQLException {
        String WHERE = "_id=" + iD;
        Cursor mCursor = this.openDBForRead().mDb.query(true, tableName, allColumns, WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    /**
     * Return a Cursor over the list of all cars in the database
     *
     * @return Cursor over all Competition Data entries
     */
    public Cursor getAllEntries(String tableName, String[] allColumns) {
        return this.openDBForRead().mDb.query(tableName, allColumns, null, null, null, null, null);
    }

    /**
     * Delete the Competition datum with the given rowId
     *
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteEntry(long rowId, String tableName) {
        boolean retVal = this.openDBForWrite().mDb.delete(tableName, "_id=" + rowId, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    public boolean deleteAllEntries(String tableName) {
        return this.openDBForWrite().mDb.delete(tableName, null, null) > 0;
    }

    public boolean setEntryExported(long rowId, String tableName) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.FALSE.toString());
        boolean retVal = this.openDBForWrite().mDb.update(tableName, args, "_id=" + rowId, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    public Cursor getAllEntriesToExport(String tableName, String[] allColumns) {
        String WHERE = COLUMN_NAME_READY_TO_EXPORT + "=" + Boolean.TRUE.toString();
        return this.openDBForRead().mDb.query(tableName, allColumns, WHERE, null, null, null, null);
    }
}
