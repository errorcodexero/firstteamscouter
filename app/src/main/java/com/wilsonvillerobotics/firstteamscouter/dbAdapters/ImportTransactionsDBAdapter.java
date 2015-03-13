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

public class ImportTransactionsDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "import_transactions";

    // Columns
    public static final String COLUMN_NAME_IMPORTED_FILE_NAME = "imported_file_name";

    public static String[] allColumns = new String[]{
    		_ID,
            COLUMN_NAME_IMPORTED_FILE_NAME
    };

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
        	FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::DatabaseHelper::onUpgrade : running onUpgrade\n");
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
    public ImportTransactionsDBAdapter(Context ctx) {
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
    public ImportTransactionsDBAdapter openForWrite() throws SQLException {
    	if(this.dbIsClosed()) {
    		if(this.mDbHelper == null) {
    			this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
    		}

    		try {
    			FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::openForWrite : GETTING WRITABLE DB\n");
    			this.mDb = this.mDbHelper.getWritableDatabase();
    		}
    		catch (SQLException e) {
    			FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::openForWrite : SQLException\n");
    			this.mDb = null;
    		}
    	} else {
    		FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::openForWrite : DB ALREADY OPEN\n");
    	}
    	return this;
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
        if(this.dbIsClosed()) {
            if(this.mDbHelper == null) {
                this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
            }

            try {
                FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::openForWrite : GETTING WRITABLE DB\n");
                this.mDb = this.mDbHelper.getReadableDatabase();
            }
            catch (SQLException e) {
                FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::openForWrite : SQLException\n");
                this.mDb = null;
            }
        } else {
            FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::openForWrite : DB ALREADY OPEN\n");
        }
        return this;
    }

    public boolean dbIsClosed() {
    	if(this.mDb == null) {
    		return true;
    	} else {
    		return !this.mDb.isOpen();
    	}
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

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * @param fileName
     * @return rowId or -1 if failed
     */
    public long fileWasImported(String fileName) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_IMPORTED_FILE_NAME, fileName);
        long id = this.openForWrite().mDb.insert(TABLE_NAME, null, args);
        if(!this.dbIsClosed()) this.close();
        return id;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     *
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllImportTransactions() {
        return this.openForRead().mDb.query(TABLE_NAME, this.allColumns, null, null, null, null, _ID);
    }

    public boolean fileHasNotBeenImported(String fileName) {
        boolean fileExists = false;
        String WHERE = COLUMN_NAME_IMPORTED_FILE_NAME + " = '" + fileName + "' ";
        Cursor c = this.openForRead().mDb.query(TABLE_NAME, this.allColumns, WHERE, null, null, null, null );
        fileExists = (c != null && c.getCount() > 1);
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
    public boolean deleteImportTransaction(long rowId) {

        return this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }
    
    public void deleteAllData()
    {
        this.openForWrite().mDb.delete(TABLE_NAME, null, null);
    }
}

