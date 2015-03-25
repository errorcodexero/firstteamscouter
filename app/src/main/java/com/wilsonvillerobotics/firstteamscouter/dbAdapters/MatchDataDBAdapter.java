package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

public class MatchDataDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME                       = "match_data";

    // Columns
    public static final String COLUMN_NAME_COMPETITION_ID       = "competition_id";
    public static final String COLUMN_NAME_MATCH_TIME 			= "match_time";
    public static final String COLUMN_NAME_MATCH_TYPE			= "match_type";
    public static final String COLUMN_NAME_MATCH_NUMBER         = "match_number";
    public static final String COLUMN_NAME_MATCH_LOCATION       = "match_location";

    public static final String COLUMN_NAME_RED_TEAM_ONE_ID      = "red_team_one_id";
    public static final String COLUMN_NAME_RED_TEAM_TWO_ID      = "red_team_two_id";
    public static final String COLUMN_NAME_RED_TEAM_THREE_ID    = "red_team_three_id";
    public static final String COLUMN_NAME_BLUE_TEAM_ONE_ID     = "blue_team_one_id";
    public static final String COLUMN_NAME_BLUE_TEAM_TWO_ID     = "blue_team_two_id";
    public static final String COLUMN_NAME_BLUE_TEAM_THREE_ID   = "blue_team_three_id";
    
    //public static final String COLUMN_NAME_MATCH_DATA_UPDATED	= "match_data_updated";
    //public static final String COLUMN_NAME_READY_TO_EXPORT      = "ready_to_export";

    public static String[] allColumns = {
            _ID,
            COLUMN_NAME_TABLET_ID,
            COLUMN_NAME_COMPETITION_ID,
            COLUMN_NAME_MATCH_TIME,
            COLUMN_NAME_MATCH_TYPE,
            COLUMN_NAME_MATCH_NUMBER,
            COLUMN_NAME_MATCH_LOCATION,
            COLUMN_NAME_RED_TEAM_ONE_ID,
            COLUMN_NAME_RED_TEAM_TWO_ID,
            COLUMN_NAME_RED_TEAM_THREE_ID,
            COLUMN_NAME_BLUE_TEAM_ONE_ID,
            COLUMN_NAME_BLUE_TEAM_TWO_ID,
            COLUMN_NAME_BLUE_TEAM_THREE_ID,
            COLUMN_NAME_READY_TO_EXPORT
    };

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
    public MatchDataDBAdapter(Context ctx) {
        super(ctx);
//        this.mCtx = ctx;
    }

    public MatchDataDBAdapter openForRead() throws SQLException {
        return (MatchDataDBAdapter)openDBForRead();
    }

    public MatchDataDBAdapter openForWrite() throws SQLException {
        return (MatchDataDBAdapter)openDBForWrite();
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     *
     * @param competition_id
     * @param match_time
     * @param match_type
     * @param match_num
     * @param red_one_id
     * @param red_two_id
     * @param red_three_id
     * @param blue_one_id
     * @param blue_two_id
     * @param blue_three_id
     * @return rowId or -1 if failed
     */
    public long createMatchData(long competition_id, String match_time, String match_type, String match_num, long red_one_id, long red_two_id, long red_three_id,
    		long blue_one_id, long blue_two_id, long blue_three_id){
        ContentValues initialValues = new ContentValues();
        initialValues.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
        initialValues.put(COLUMN_NAME_COMPETITION_ID, competition_id);
        initialValues.put(COLUMN_NAME_MATCH_TIME, match_time);
        initialValues.put(COLUMN_NAME_MATCH_TYPE, match_type);
        initialValues.put(COLUMN_NAME_MATCH_NUMBER, match_num);
        initialValues.put(COLUMN_NAME_RED_TEAM_ONE_ID, red_one_id);
        initialValues.put(COLUMN_NAME_RED_TEAM_TWO_ID, red_two_id);
        initialValues.put(COLUMN_NAME_RED_TEAM_THREE_ID, red_three_id);
        initialValues.put(COLUMN_NAME_BLUE_TEAM_ONE_ID, blue_one_id);
        initialValues.put(COLUMN_NAME_BLUE_TEAM_TWO_ID, blue_two_id);
        initialValues.put(COLUMN_NAME_BLUE_TEAM_THREE_ID, blue_three_id);
        //initialValues.put(COLUMN_NAME_MATCH_DATA_UPDATED, Boolean.TRUE.toString());
        initialValues.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());

        long id = this.openForWrite().mDb.insert(TABLE_NAME, null, initialValues);
        if(!this.dbIsClosed()) this.close();
        return id;
    }

    public long createMatchData(long competition_id, int match_num){
        ContentValues initialValues = new ContentValues();
        initialValues.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
        initialValues.put(COLUMN_NAME_COMPETITION_ID, competition_id);
        initialValues.put(COLUMN_NAME_MATCH_NUMBER, match_num);
        //initialValues.put(COLUMN_NAME_MATCH_DATA_UPDATED, Boolean.TRUE.toString());
        initialValues.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());

        return this.openForWrite().mDb.insert(TABLE_NAME, null, initialValues);
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param matchId
     * @return true if deleted, false otherwise
     */
    @Override
    public boolean deleteEntry(long rowId) {
        return super.deleteEntry(rowId, TABLE_NAME);
        /*
        return this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + matchId, null) > 0;
        */
    }

    @Override
    public boolean deleteAllEntries() {
        return super.deleteAllEntries(TABLE_NAME);
        //return this.openForWrite().mDb.delete(TABLE_NAME, null, null) > 0;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    @Override
    public Cursor getAllEntries() {
        return super.getAllEntries(TABLE_NAME, allColumns);
        //return this.openForRead().mDb.query(TABLE_NAME, allColumns , null, null, null, null, null);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param matchID
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    @Override
    public Cursor getEntry(long iD) throws SQLException {
        return super.getEntry(iD, TABLE_NAME, allColumns);
        /*
    	FTSUtilities.printToConsole("MatchDataDBAdapter::getEntry : matchID: " + matchID + "\n");
		String WHERE = _ID + "=" + matchID;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns, WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
            FTSUtilities.printToConsole("MatchDataDBAdapter::getEntry : numItems: " + mCursor.getCount() + "\n");
        } else {
        	FTSUtilities.printToConsole("MatchDataDBAdapter::getEntry : Cursor is NULL\n");
        }
        return mCursor;
        */
    }

    /**
     * Return a Cursor containing all entries with updated data
     * @return Cursor of all updated entries
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getUpdatedMatchDataEntries() throws SQLException {

        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns,
        		//COLUMN_NAME_MATCH_DATA_UPDATED + "=" + Boolean.TRUE.toString(), null, null, null, null, null);
                COLUMN_NAME_READY_TO_EXPORT + "=" + Boolean.TRUE.toString(), null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    /**
     * Update the entry.
     * 
     * @param rowId
     * @param competition_id
     * @param match_time
     * @param match_type
     * @param match_num
     * @param red_one_id
     * @param red_two_id
     * @param red_three_id
     * @param blue_one_id
     * @param blue_two_id
     * @param blue_three_id
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateMatchDataEntry(long rowId, long competition_id, String match_time, String match_type, int match_num, int red_one_id, int red_two_id, int red_three_id,
    		int blue_one_id, int blue_two_id, int blue_three_id, Boolean export){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_COMPETITION_ID, competition_id);
    	args.put(COLUMN_NAME_MATCH_TIME, match_time);
    	args.put(COLUMN_NAME_MATCH_TYPE, match_type);
    	args.put(COLUMN_NAME_MATCH_NUMBER, match_num);
    	args.put(COLUMN_NAME_RED_TEAM_ONE_ID, red_one_id);
    	args.put(COLUMN_NAME_RED_TEAM_TWO_ID, red_two_id);
    	args.put(COLUMN_NAME_RED_TEAM_THREE_ID, red_three_id);
    	args.put(COLUMN_NAME_BLUE_TEAM_ONE_ID, blue_one_id);
    	args.put(COLUMN_NAME_BLUE_TEAM_TWO_ID, blue_two_id);
    	args.put(COLUMN_NAME_BLUE_TEAM_THREE_ID, blue_three_id);
    	//args.put(COLUMN_NAME_MATCH_DATA_UPDATED, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_READY_TO_EXPORT, String.valueOf(export));

        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0;
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
    
    public Cursor getTeamIDsForMatchByAlliancePosition(long competition_id, long matchID) {
        /*
    	String SELECT_QUERY = "SELECT t1." + MatchDataDBAdapter._ID;
    	SELECT_QUERY += ", t1." + MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER;
    	SELECT_QUERY += ", t1." + MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_ONE_ID;
    	SELECT_QUERY += ", t1." + MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_TWO_ID;
    	SELECT_QUERY += ", t1." + MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_THREE_ID;
    	SELECT_QUERY += ", t1." + MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_ONE_ID;
    	SELECT_QUERY += ", t1." + MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_TWO_ID;
    	SELECT_QUERY += ", t1." + MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_THREE_ID;
		SELECT_QUERY += " FROM " + MatchDataDBAdapter.TABLE_NAME + " AS t1";
    	SELECT_QUERY += " WHERE t1." + MatchDataDBAdapter._ID + "=" + matchID;
        SELECT_QUERY += " AND t1." + MatchDataDBAdapter.COLUMN_NAME_COMPETITION_ID + "=" + competition_id;
    	Cursor c = this.openForRead().mDb.rawQuery(SELECT_QUERY, null);
        */
        String WHERE = _ID + "=" + matchID;
        Cursor c = this.openForRead().mDb.query(true, TABLE_NAME, allColumns, WHERE, null, null, null, null, null);
        if(c != null) c.moveToFirst();
    	
    	return c;
    }
    
	public boolean setTeamIDsForMatchID(long competition_id, long matchID, long teamIDs[]) {
		FTSUtilities.printToConsole("MatchDataDBAdapter::setTeamIDsForMatchID : matchID: " + matchID + "  numTeamIDs: " + teamIDs.length + "\n");
		ContentValues args = new ContentValues();
    	args.put(COLUMN_NAME_RED_TEAM_ONE_ID, teamIDs[0]);
    	args.put(COLUMN_NAME_RED_TEAM_TWO_ID, teamIDs[1]);
    	args.put(COLUMN_NAME_RED_TEAM_THREE_ID, teamIDs[2]);
    	args.put(COLUMN_NAME_BLUE_TEAM_ONE_ID, teamIDs[3]);
    	args.put(COLUMN_NAME_BLUE_TEAM_TWO_ID, teamIDs[4]);
    	args.put(COLUMN_NAME_BLUE_TEAM_THREE_ID, teamIDs[5]);
    	//args.put(COLUMN_NAME_MATCH_DATA_UPDATED, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
    	
    	Cursor c = getEntry(matchID);
    	if(c.getCount() > 0) {
    		FTSUtilities.printToConsole("MatchDataDBAdapter::setTeamIDsForMatchID : Found " + c.getCount() + " record(s) for matchID: " + matchID + "\n");
            String WHERE = _ID + "=" + matchID;
            WHERE += " AND " + COLUMN_NAME_COMPETITION_ID + "=" + competition_id;
            boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, WHERE, null) >0;
            if(!this.dbIsClosed()) this.close();
    		return retVal;
    	} else {
    		FTSUtilities.printToConsole("MatchDataDBAdapter::setTeamIDsForMatchID : NO RECORD FOUND FOR matchID: " + matchID + "\n");
    		return false;
    	}
    	
	}

	public long[] populateTestData(int numMatches) {
    	FTSUtilities.printToConsole("MatchDataDBAdapter::populateTestData\n");

    	//this.deleteAllEntries();
    	
    	long matchIDs[] = new long[numMatches];

    	for(int i = 0; i < numMatches; i++) {
    		matchIDs[i] = this.createMatchData(0, i + 1);
    	}
    	return matchIDs;
    }
}
