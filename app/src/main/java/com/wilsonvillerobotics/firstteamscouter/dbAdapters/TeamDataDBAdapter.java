package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import java.util.Set;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

public class TeamDataDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME = "team_data";

    // Primary Key comprised of two columns
    public static final String COLUMN_NAME_TEAM_NUMBER = "team_number";
    public static final String COLUMN_NAME_TEAM_SUB_NUMBER = "team_sub_number";


    public static final String COLUMN_NAME_TEAM_NAME = "team_name";
    public static final String COLUMN_NAME_TEAM_CITY = "team_city";
    public static final String COLUMN_NAME_TEAM_STATE = "team_state";
    public static final String COLUMN_NAME_TEAM_NUM_MEMBERS = "num_team_members";
    public static final String COLUMN_NAME_TEAM_YEAR_CREATED = "team_creation_year";

    //public static final String PRIMARY_KEY = " PRIMARY KEY ( " + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + ", " + TeamDataDBAdapter.COLUMN_NAME_TEAM_SUB_NUMBER + " )";

    public static String[] allColumns = {
            _ID,
            COLUMN_NAME_TABLET_ID,
            COLUMN_NAME_TEAM_NUMBER,
            COLUMN_NAME_TEAM_SUB_NUMBER,
            COLUMN_NAME_TEAM_NAME,
            COLUMN_NAME_TEAM_CITY,
            COLUMN_NAME_TEAM_STATE,
            COLUMN_NAME_TEAM_NUM_MEMBERS,
            COLUMN_NAME_TEAM_YEAR_CREATED,
            //COLUMN_NAME_TEAM_DATA_UPDATED,
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
    public TeamDataDBAdapter(Context ctx) {
        super(ctx);
    	FTSUtilities.printToConsole("Constructor::TeamDataDBAdapter");
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
    public TeamDataDBAdapter openForWrite() throws SQLException {
        return (TeamDataDBAdapter)openDBForWrite();
        /*
    	FTSUtilities.printToConsole("Opening TeamDataDBAdapter Database");
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getWritableDatabase();
        FTSUtilities.printToConsole("TeamDataDBAdapter::openForWrite : DB " + ((mDb == null) ? "IS" : "Is Not") + " null");
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
     * @throws SQLException
     *             if the database could be neither opened or created
     */
    public TeamDataDBAdapter openForRead() throws SQLException {
        return (TeamDataDBAdapter)openDBForRead();
        /*
        FTSUtilities.printToConsole("Opening TeamDataDBAdapter Database");
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getReadableDatabase();
        FTSUtilities.printToConsole("TeamDataDBAdapter::openForRead : DB " + ((mDb == null) ? "IS" : "Is Not") + " null");
        return this;
        */
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * 
     * @param team_number
     * @param team_name
     * @param team_city
     * @param num_team_members
     * @return rowId or -1 if failed
     */
    public long createTeamDataEntry(int team_number, int team_sub_number, String team_name, String team_city, String team_state, int num_team_members){
        long retVal = -1;
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
        args.put(COLUMN_NAME_TEAM_NUMBER, team_number);
        args.put(COLUMN_NAME_TEAM_SUB_NUMBER, team_sub_number);
        args.put(COLUMN_NAME_TEAM_NAME, team_name);
        args.put(COLUMN_NAME_TEAM_CITY, team_city);
        args.put(COLUMN_NAME_TEAM_STATE, team_state);
        args.put(COLUMN_NAME_TEAM_NUM_MEMBERS, num_team_members);
        //args.put(COLUMN_NAME_TEAM_DATA_UPDATED, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
        boolean success = this.openForWrite().mDb.insert(TABLE_NAME, null, args) > 0;
        if(success) retVal = team_number;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    public int[] createTeamDataEntryIfNotExist(int team_number, int team_sub_number) {
    	Cursor c = null;
    	int retVal[] = {-1,-1};
    	try {
    		c = this.getTeamEntry(team_number, team_sub_number);
            if(c != null && c.moveToFirst()) {
                retVal[0] = c.getInt(c.getColumnIndex(COLUMN_NAME_TEAM_NUMBER));
                retVal[1] = c.getInt(c.getColumnIndex(COLUMN_NAME_TEAM_SUB_NUMBER));
            }    	}
    	catch(Exception e) {
    		ContentValues args = new ContentValues();
            args.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
            args.put(COLUMN_NAME_TEAM_NUMBER, team_number);
            args.put(COLUMN_NAME_TEAM_SUB_NUMBER, team_sub_number);
            //args.put(COLUMN_NAME_TEAM_DATA_UPDATED, Boolean.TRUE.toString());
            args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
            long id = this.openForWrite().mDb.insert(TABLE_NAME, null, args);
            if(id != -1) {
                retVal[0] = team_number;
                retVal[1] = team_sub_number;
            }
    	}
        if(c != null && !c.isClosed()) c.close();
        if(!this.dbIsClosed()) this.close();
        return retVal; 
    }

    /**
     * Update the entry.
     * 
     * @param team_number
     * @param team_sub_number
     * @param team_name
     * @param team_city
     * @param num_team_members
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateTeamDataEntry(long teamID, int team_number, int team_sub_number, String team_name,
    		String team_city, String team_state, int num_team_members, Boolean export){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_NUMBER, team_number);
        args.put(COLUMN_NAME_TEAM_SUB_NUMBER, team_sub_number);
        args.put(COLUMN_NAME_TEAM_NAME, team_name);
        args.put(COLUMN_NAME_TEAM_CITY, team_city);
        args.put(COLUMN_NAME_TEAM_STATE, team_state);
        args.put(COLUMN_NAME_TEAM_NUM_MEMBERS, num_team_members);
        //args.put(COLUMN_NAME_TEAM_DATA_UPDATED, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_READY_TO_EXPORT, String.valueOf(export));
        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, _ID + "=" + teamID, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    /**
     * Delete the entry with the given rowId
     *
     * @param rowID
     * @return true if deleted, false otherwise
     */
    @Override
    public boolean deleteEntry(long rowID) {
        return super.deleteEntry(rowID, TABLE_NAME);
    }

    public boolean deleteAllEntries()
    {
        return super.deleteAllEntries(TABLE_NAME);
        //this.openForWrite().mDb.delete(TABLE_NAME, null, null);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowID
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getEntry(long rowID) throws SQLException {
        return super.getEntry(rowID, TABLE_NAME, allColumns);
    }

    public Cursor getTeamEntry(int team_number, int team_sub_number) {
        String WHERE = COLUMN_NAME_TEAM_NUMBER + "=" + String.valueOf(team_number);
        WHERE += " AND " + COLUMN_NAME_TEAM_SUB_NUMBER + "=" + String.valueOf(team_sub_number);
        return this.openForRead().mDb.query(TABLE_NAME, allColumns, WHERE, null, null, null, null);
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    @Override
    public Cursor getAllEntries() {
        return super.getAllEntries(TABLE_NAME, allColumns);
        /*
        String SORT = COLUMN_NAME_TEAM_NUMBER + " ASC";
        Cursor mCursor = this.openForRead().mDb.query(TABLE_NAME, allColumns, null, null, null, null, SORT);
        FTSUtilities.printToConsole("TeamDataDBAdapter::getAllEntries : Cursor Size : " + mCursor.getCount() + "\n");
        return mCursor;
        */
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param //teamID
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    /*  NO LONGER VALID NOW THAT TEAM NUMBER IS PART OF THE ID
    public int getTeamNumberFromID(long teamID) throws SQLException {
    	int teamNum = -1;

    	Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, new String[] { COLUMN_NAME_TEAM_NUMBER},
        		_ID + "=" + teamID, null, null, null, null, null);
        if (mCursor != null && mCursor.moveToFirst()) {
            teamNum = mCursor.getInt(mCursor.getColumnIndex(COLUMN_NAME_TEAM_NUMBER));
        }
        return teamNum;
    }
    */

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
    
    public long[] populateTestData() {
    	FTSUtilities.printToConsole("TeamDataDBAdapter::populateTestData\n");
    	//deleteAllEntries();
    	
    	Set<Integer> teamNums = FTSUtilities.getTestTeamNumbers();
    	long teamIDs[] = new long[teamNums.size()];
    	int i = 0;
    	
    	for(int teamNum : teamNums) {
    		teamIDs[i++] = this.createTeamDataEntry(teamNum, 0, FTSUtilities.getTeamName(teamNum), "City", "State", 42);
    	}
    	return teamIDs;
    }
}
