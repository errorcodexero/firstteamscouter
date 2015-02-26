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
import java.util.HashMap;

public class TeamMatchTransactionsDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "team_match_transactions";
    public static final String COLUMN_NAME_TEAM_MATCH_ID = "team_match_id";
    public static final String COLUMN_NAME_TRANSACTION_ID = "transaction_id";

    private String[] allColumnNames = new String[]{
    		_ID,
            COLUMN_NAME_TEAM_MATCH_ID,
            COLUMN_NAME_TRANSACTION_ID
    };

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
    public TeamMatchTransactionsDBAdapter(Context ctx) {
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
    public TeamMatchTransactionsDBAdapter open() throws SQLException {
    	if(this.dbNotOpen()) {
    		if(this.mDbHelper == null) {
    			this.mDbHelper = new DatabaseHelper(this.mCtx);
    		}

    		try {
    			FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::open : GETTING WRITABLE DB\n");
    			this.mDb = this.mDbHelper.getWritableDatabase();
    		}
    		catch (SQLException e) {
    			FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::open : SQLException\n");
    			this.mDb = null;
    		}
    	} else {
    		FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::open : DB ALREADY OPEN\n");
    	}
    	return this;
    }

    public boolean dbNotOpen() {
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
        this.mDbHelper.close();
        this.mDb = null;
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * @param teamMatchId
     * @param transactionId
     * @return rowId or -1 if failed
     */
    public long createTeamMatchTransaction(long teamMatchId, long transactionId) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_MATCH_ID, teamMatchId);
        args.put(COLUMN_NAME_TRANSACTION_ID, transactionId);
        return this.mDb.insert(TABLE_NAME, null, args);
    }

    /**
     * Return a Cursor over the list of all entries in the database
     *
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllTeamMatchTransactions() {
        return this.mDb.query(TABLE_NAME, this.allColumnNames, null, null, null, null, _ID);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws android.database.SQLException if entry could not be found/retrieved
     */
    public Long getTeamMatchTransactionId(long rowId) throws SQLException {
        Cursor mCursor = null;
        Long transactionId = -1l;
        try {
            mCursor = this.mDb.query(true, TABLE_NAME, this.allColumnNames,
                    _ID + "=" + rowId, null, null, null, null, null);
            mCursor.moveToFirst();
            transactionId = mCursor.getLong(mCursor.getColumnIndex(COLUMN_NAME_TRANSACTION_ID));
        }
        catch (Exception e) {
            FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::getTeamMatchTransactionId : No transaction for ID " + rowId + " was found.");
        }
        return transactionId;
    }

    /**
     * Return a Cursor over all transactions for a given TeamMatch ID
     * @param teamMatchID
     * @return Cursor over all transactions for a given TeamMatch ID, if found
     * @throws android.database.SQLException if entry could not be found/retrieved
     */
    public Long[] getTransactionIdsForTeamMatch(long teamMatchID) {
        String selection = COLUMN_NAME_TEAM_MATCH_ID + "=" + String.valueOf(teamMatchID);
        Cursor mCursor = null;
        ArrayList<Long> transactionIds = new ArrayList<Long>();
        try {
            mCursor = this.mDb.query(TABLE_NAME, this.allColumnNames, selection, null, null, null, _ID);
            while(mCursor.moveToNext()) {
                transactionIds.add(mCursor.getLong(mCursor.getColumnIndex(COLUMN_NAME_TRANSACTION_ID)));
            }
        }
        catch (Exception e) {
            FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::getTransactionIdsForTeamMatch : No transactions for TeamMatch " + teamMatchID + " were found.");
        }
        return transactionIds.toArray(new Long[0]);
    }

    // TODO - Make this work as intended - get all transactions for a teamMatchID - need to tie in query to transaction table
    /**
     * Return a Cursor over all transactions for a given TeamMatch ID
     * @param teamMatchID
     * @return Cursor over all transactions for a given TeamMatch ID, if found
     * @throws android.database.SQLException if entry could not be found/retrieved
     */
    public Cursor getTransactionsForTeamMatch(long teamMatchID) {
        String selection = _ID + "=" + String.valueOf(teamMatchID);
        Cursor mCursor = null;
        try {
            mCursor = this.mDb.query(TABLE_NAME, this.allColumnNames, selection, null, null, null, _ID);
            mCursor.moveToFirst();
        }
        catch (Exception e) {
            FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::getTransactionForTeamMatch : No transactions for TeamMatch " + teamMatchID + " were found.");
        }
        return mCursor;
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteTeamMatchTransaction(long rowId) {

        return this.mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }
    
    public void deleteAllData()
    {
        mDb.delete(TABLE_NAME, null, null);
    }

    public boolean populateTestData(long[] matchIDs, long[] teamIDs) {
    	FTSUtilities.printToConsole("TeamMatchTransactionsDBAdapter::populateTestData\n");
    	deleteAllData();

    	return false;
    }
}

