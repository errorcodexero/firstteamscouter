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

public class TeamMatchTransactionDataDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME = "team_match_transaction";

    // Columns
    public static final String COLUMN_NAME_TEAM_ID = "team_id";
    public static final String COLUMN_NAME_MATCH_ID = "match_id";
    public static final String COLUMN_NAME_TIMESTAMP = "transaction_timestamp";
    public static final String COLUMN_NAME_ACTION = "action_name";
    public static final String COLUMN_NAME_ACTION_PHASE = "action_phase";
    public static final String COLUMN_NAME_ACTION_START_LOCATION_NAME = "action_start_location_name";
    public static final String COLUMN_NAME_ACTION_START_LOCATION_X = "action_start_location_X";
    public static final String COLUMN_NAME_ACTION_START_LOCATION_Y = "action_start_location_Y";
    public static final String COLUMN_NAME_ACTION_END_LOCATION_NAME = "action_end_location_name";
    public static final String COLUMN_NAME_ACTION_END_LOCATION_X = "action_end_location_X";
    public static final String COLUMN_NAME_ACTION_END_LOCATION_Y = "action_end_location_Y";
    public static final String COLUMN_NAME_ELEMENT_TYPES = "element_types";
    public static final String COLUMN_NAME_ELEMENT_STATES = "element_states";

    public static String[] allColumns = new String[]{
    		_ID,
            COLUMN_NAME_TABLET_ID,
    	    COLUMN_NAME_TEAM_ID,
    	    COLUMN_NAME_MATCH_ID,
            COLUMN_NAME_TIMESTAMP,
            COLUMN_NAME_ACTION,
            COLUMN_NAME_ACTION_PHASE,
            COLUMN_NAME_ACTION_START_LOCATION_NAME,
            COLUMN_NAME_ACTION_START_LOCATION_X,
            COLUMN_NAME_ACTION_START_LOCATION_Y,
            COLUMN_NAME_ACTION_END_LOCATION_NAME,
            COLUMN_NAME_ACTION_END_LOCATION_X,
            COLUMN_NAME_ACTION_END_LOCATION_Y,
            COLUMN_NAME_ELEMENT_TYPES,
            COLUMN_NAME_ELEMENT_STATES,
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
    public TeamMatchTransactionDataDBAdapter(Context ctx) {
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
     * @throws android.database.SQLException
     *             if the database could be neither opened or created
     */
    public TeamMatchTransactionDataDBAdapter openForWrite() throws SQLException {
        return (TeamMatchTransactionDataDBAdapter)openDBForWrite();
        /*
    	if(this.dbIsClosed()) {
    		if(this.mDbHelper == null) {
    			this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
    		}

    		try {
    			FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::openForWrite : GETTING WRITABLE DB\n");
    			this.mDb = this.mDbHelper.getWritableDatabase();
    		}
    		catch (SQLException e) {
    			FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::openForWrite : SQLException\n");
    			this.mDb = null;
    		}
    	} else {
    		FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::openForWrite : DB ALREADY OPEN\n");
    	}
    	return this;
    	*/
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
    public TeamMatchTransactionDataDBAdapter openForRead() throws SQLException {
        return (TeamMatchTransactionDataDBAdapter)openDBForRead();
        /*
        if(this.dbIsClosed()) {
            if(this.mDbHelper == null) {
                this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
            }

            try {
                FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::openForRead : GETTING READABLE DB\n");
                this.mDb = this.mDbHelper.getReadableDatabase();
            }
            catch (SQLException e) {
                FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::openForRead : SQLException\n");
                this.mDb = null;
            }
        } else {
            FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::openForRead : DB ALREADY OPEN\n");
        }
        return this;
        */
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * @param values A HashMap of Objects, inserted using column names as keys.
     * @return rowId or -1 if failed
     */
    public long createTeamMatchDataTransaction(HashMap<String, Object> values) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
        args.put(COLUMN_NAME_TEAM_ID, String.valueOf(values.get(COLUMN_NAME_TEAM_ID)));
        args.put(COLUMN_NAME_MATCH_ID, String.valueOf(values.get(COLUMN_NAME_MATCH_ID)));
        args.put(COLUMN_NAME_TIMESTAMP, String.valueOf(values.get(COLUMN_NAME_TIMESTAMP)));
        args.put(COLUMN_NAME_ACTION, String.valueOf(values.get(COLUMN_NAME_ACTION)));
        args.put(COLUMN_NAME_ACTION_PHASE, String.valueOf(values.get(COLUMN_NAME_ACTION_PHASE)));
        args.put(COLUMN_NAME_ACTION_START_LOCATION_NAME, String.valueOf(values.get(COLUMN_NAME_ACTION_START_LOCATION_NAME)));
        args.put(COLUMN_NAME_ACTION_START_LOCATION_X, String.valueOf(values.get(COLUMN_NAME_ACTION_START_LOCATION_X)));
        args.put(COLUMN_NAME_ACTION_START_LOCATION_Y, String.valueOf(values.get(COLUMN_NAME_ACTION_START_LOCATION_Y)));
        args.put(COLUMN_NAME_ACTION_END_LOCATION_NAME, String.valueOf(values.get(COLUMN_NAME_ACTION_END_LOCATION_NAME)));
        args.put(COLUMN_NAME_ACTION_END_LOCATION_X, String.valueOf(values.get(COLUMN_NAME_ACTION_END_LOCATION_X)));
        args.put(COLUMN_NAME_ACTION_END_LOCATION_Y, String.valueOf(values.get(COLUMN_NAME_ACTION_END_LOCATION_Y)));
        args.put(COLUMN_NAME_ELEMENT_TYPES, String.valueOf(values.get(COLUMN_NAME_ELEMENT_TYPES)));
        args.put(COLUMN_NAME_ELEMENT_STATES, String.valueOf(values.get(COLUMN_NAME_ELEMENT_STATES)));
        args.put(COLUMN_NAME_READY_TO_EXPORT, String.valueOf(values.get(COLUMN_NAME_READY_TO_EXPORT)));
        long id = this.openForWrite().mDb.insert(TABLE_NAME, null, args);
        if(!this.dbIsClosed()) this.close();
        return id;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws android.database.SQLException if entry could not be found/retrieved
     */
    @Override
    public Cursor getEntry(long rowId) throws SQLException {
        Cursor mCursor = null;
        try {
            mCursor = this.openForRead().mDb.query(true, TABLE_NAME, this.allColumns,
                    _ID + "=" + rowId, null, null, null, null, null);
            mCursor.moveToFirst();
        }
        catch (Exception e) {
            FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::getEntry : No transaction for ID " + rowId + " was found.");
        }
        return mCursor;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     *
     * @return Cursor over all entries
     */
    @Override
    public Cursor getAllEntries() {
        return super.getAllEntries(TABLE_NAME, allColumns);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @return Cursor positioned to matching entry, if found
     * @throws android.database.SQLException if entry could not be found/retrieved
     */
    public Long[] getTeamMatchTransactionIDs(long matchID, long teamID) throws SQLException {
    	String WHERE = TeamMatchTransactionDataDBAdapter.COLUMN_NAME_MATCH_ID + "=" + matchID;
    	WHERE += " AND " + TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TEAM_ID + "=" + teamID;
        Cursor mCursor = null;
        ArrayList<Long> transactionIDs = new ArrayList<Long>();
        try {
            mCursor = this.openForRead().mDb.query(true, TABLE_NAME, this.allColumns, WHERE, null, null, null, null, _ID);
            while(mCursor.moveToNext()) {
                transactionIDs.add(mCursor.getLong(mCursor.getColumnIndex(_ID)));
            }
        }
        catch (Exception e) {
            FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::getTeamMatchTransactionIDs : No transaction for teamID " + teamID + " and matchID " + matchID + " was found.");
        } finally {
            if(mCursor != null && !mCursor.isClosed()) mCursor.close();
            if(!this.dbIsClosed()) this.close();
        }
        return transactionIDs.toArray(new Long[0]);
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
        //return this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
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

    @Override
    public boolean deleteAllEntries() {
        return super.deleteAllEntries(TABLE_NAME);
        //this.openForWrite().mDb.delete(TABLE_NAME, null, null);
    }

    public boolean populateTestData(long[] matchIDs, long[] teamIDs) {
    	FTSUtilities.printToConsole("TeamMatchTransactionDBAdapter::populateTestData\n");
    	//deleteAllEntries();

    	return false;
    }
}

