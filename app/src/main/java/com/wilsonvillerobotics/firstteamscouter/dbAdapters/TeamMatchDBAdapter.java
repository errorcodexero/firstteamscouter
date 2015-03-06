package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

public class TeamMatchDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "team_match";
    public static final String COLUMN_NAME_TEAM_ID = "team_id";
    public static final String COLUMN_NAME_MATCH_ID = "match_id";
    public static final String COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION = "alliance_position";
    public static final String COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT = "data_ready_to_export";
    public static final String COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA = "team_match_has_saved_data";
    public static final String COLUMN_NAME_BROKE_DOWN = "broke_down";
    public static final String COLUMN_NAME_NO_MOVE = "no_move";
    public static final String COLUMN_NAME_LOST_CONNECTION = "lost_connection";
    public static final String COLUMN_NAME_START_LOCATION = "starting_location";
    public static final String COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X = "starting_location_X";
    public static final String COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y = "starting_location_Y";
    public static final String COLUMN_NAME_START_LOCATION_ON_FIELD = "starting_location_on_field";
    public static final String COLUMN_NAME_AUTO_TOTES_PICKED_UP = "auto_totes_picked_up";
    public static final String COLUMN_NAME_AUTO_TOTES_STACKED = "auto_totes_stacked";
    public static final String COLUMN_NAME_AUTO_TOTES_SCORED = "auto_totes_scored";
    public static final String COLUMN_NAME_AUTO_CANS_PICKED_UP = "auto_cans_picked_up";
    public static final String COLUMN_NAME_AUTO_CANS_SCORED = "auto_cans_scored";
    public static final String COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP = "auto_cans_pulled_from_step";
    public static final String COLUMN_NAME_AUTO_MODE_SAVED = "auto_mode_saved";
    public static final String COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X = "auto_final_location_X";
    public static final String COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y = "auto_final_location_Y";
    public static final String COLUMN_NAME_AUTO_TOTE_1_LOCATION_X = "auto_tote_1_location_X";
    public static final String COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y = "auto_tote_1_location_Y";
    public static final String COLUMN_NAME_AUTO_TOTE_2_LOCATION_X = "auto_tote_2_location_X";
    public static final String COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y = "auto_tote_2_location_Y";
    public static final String COLUMN_NAME_AUTO_TOTE_3_LOCATION_X = "auto_tote_3_location_X";
    public static final String COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y = "auto_tote_3_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_1_LOCATION_X = "auto_can_1_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_1_LOCATION_Y = "auto_can_1_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_2_LOCATION_X = "auto_can_2_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_2_LOCATION_Y = "auto_can_2_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_3_LOCATION_X = "auto_can_3_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_3_LOCATION_Y = "auto_can_3_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_4_LOCATION_X = "auto_can_4_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_4_LOCATION_Y = "auto_can_4_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_5_LOCATION_X = "auto_can_5_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_5_LOCATION_Y = "auto_can_5_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_6_LOCATION_X = "auto_can_6_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_6_LOCATION_Y = "auto_can_6_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_7_LOCATION_X = "auto_can_7_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_7_LOCATION_Y = "auto_can_7_location_Y";
    public static final String COLUMN_NAME_AUTO_ROBOT_VISIBLE = "auto_robot_visible";
    public static final String COLUMN_NAME_AUTO_TOTE1_VISIBLE = "auto_tote1_visible";
    public static final String COLUMN_NAME_AUTO_TOTE2_VISIBLE = "auto_tote2_visible";
    public static final String COLUMN_NAME_AUTO_TOTE3_VISIBLE = "auto_tote3_visible";
    public static final String COLUMN_NAME_AUTO_CAN1_VISIBLE = "auto_can1_visible";
    public static final String COLUMN_NAME_AUTO_CAN2_VISIBLE = "auto_can2_visible";
    public static final String COLUMN_NAME_AUTO_CAN3_VISIBLE = "auto_can3_visible";
    public static final String COLUMN_NAME_AUTO_CAN4_VISIBLE = "auto_can4_visible";
    public static final String COLUMN_NAME_AUTO_CAN5_VISIBLE = "auto_can5_visible";
    public static final String COLUMN_NAME_AUTO_CAN6_VISIBLE = "auto_can6_visible";
    public static final String COLUMN_NAME_AUTO_CAN7_VISIBLE = "auto_can7_visible";
    public static final String COLUMN_NAME_AUTO_ROBOT_STACK_LIST = "auto_robot_stack_list";

    // This needs to be moved to the team_match_notes_data
    public static final String COLUMN_NAME_TEAM_MATCH_NOTES = "team_match_notes";


    private String[] allColumnNames = new String[]{
    		_ID,
    	    COLUMN_NAME_TEAM_ID,
    	    COLUMN_NAME_MATCH_ID,
    	    COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION,
    	    COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT,
    	    COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA,
    	    COLUMN_NAME_BROKE_DOWN,
    	    COLUMN_NAME_NO_MOVE,
    	    COLUMN_NAME_LOST_CONNECTION,
    	    COLUMN_NAME_START_LOCATION,
            COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X,
            COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y,
            COLUMN_NAME_AUTO_MODE_SAVED,
            COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X,
            COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y,
            COLUMN_NAME_AUTO_TOTE_1_LOCATION_X,
            COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y,
            COLUMN_NAME_AUTO_TOTE_2_LOCATION_X,
            COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y,
            COLUMN_NAME_AUTO_TOTE_3_LOCATION_X,
            COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y,
            COLUMN_NAME_AUTO_CAN_1_LOCATION_X,
            COLUMN_NAME_AUTO_CAN_1_LOCATION_Y,
            COLUMN_NAME_AUTO_CAN_2_LOCATION_X,
            COLUMN_NAME_AUTO_CAN_2_LOCATION_Y,
            COLUMN_NAME_AUTO_CAN_3_LOCATION_X,
            COLUMN_NAME_AUTO_CAN_3_LOCATION_Y,
            COLUMN_NAME_AUTO_CAN_4_LOCATION_X,
            COLUMN_NAME_AUTO_CAN_4_LOCATION_Y,
            COLUMN_NAME_AUTO_CAN_5_LOCATION_X,
            COLUMN_NAME_AUTO_CAN_5_LOCATION_Y,
            COLUMN_NAME_AUTO_CAN_6_LOCATION_X,
            COLUMN_NAME_AUTO_CAN_6_LOCATION_Y,
            COLUMN_NAME_AUTO_CAN_7_LOCATION_X,
            COLUMN_NAME_AUTO_CAN_7_LOCATION_Y,
            COLUMN_NAME_AUTO_ROBOT_VISIBLE,
            COLUMN_NAME_AUTO_TOTE1_VISIBLE,
            COLUMN_NAME_AUTO_TOTE2_VISIBLE,
            COLUMN_NAME_AUTO_TOTE3_VISIBLE,
            COLUMN_NAME_AUTO_CAN1_VISIBLE,
            COLUMN_NAME_AUTO_CAN2_VISIBLE,
            COLUMN_NAME_AUTO_CAN3_VISIBLE,
            COLUMN_NAME_AUTO_CAN4_VISIBLE,
            COLUMN_NAME_AUTO_CAN5_VISIBLE,
            COLUMN_NAME_AUTO_CAN6_VISIBLE,
            COLUMN_NAME_AUTO_CAN7_VISIBLE,
            COLUMN_NAME_AUTO_ROBOT_STACK_LIST,
            COLUMN_NAME_AUTO_TOTES_PICKED_UP,
            COLUMN_NAME_AUTO_TOTES_STACKED,
            COLUMN_NAME_AUTO_TOTES_SCORED,
            COLUMN_NAME_AUTO_CANS_PICKED_UP,
            COLUMN_NAME_AUTO_CANS_SCORED,
            COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP,
            COLUMN_NAME_START_LOCATION_ON_FIELD,
    	    COLUMN_NAME_TEAM_MATCH_NOTES
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
        	FTSUtilities.printToConsole("TeamMatchDBAdapter::DatabaseHelper::onUpgrade : running onUpgrade\n");
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
    public TeamMatchDBAdapter(Context ctx) {
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
    public TeamMatchDBAdapter open() throws SQLException {
    	if(this.dbNotOpen()) {
    		if(this.mDbHelper == null) {
    			this.mDbHelper = new DatabaseHelper(this.mCtx);
    		}

    		try {
    			FTSUtilities.printToConsole("TeamMatchDBAdapter::open : GETTING WRITABLE DB\n");
    			this.mDb = this.mDbHelper.getWritableDatabase();
    		}
    		catch (SQLException e) {
    			FTSUtilities.printToConsole("TeamMatchDBAdapter::open : SQLException\n");
    			this.mDb = null;
    		}
    	} else {
    		FTSUtilities.printToConsole("TeamMatchDBAdapter::open : DB ALREADY OPEN\n");
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
     * @param alliancePosition 
     * @param team_id
     * @param match_id
     * @return rowId or -1 if failed
     */
    public long createTeamMatch(String alliancePosition, long team_id, long match_id) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION, alliancePosition);
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_MATCH_ID, String.valueOf(match_id));
        args.put(COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_MODE_SAVED, Boolean.FALSE.toString());
        return this.mDb.insert(TABLE_NAME, null, args);
    }

    /**
     * Update the entry.
     * 
     * @param team_id team ID
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateTeamMatch(long team_match_id, long team_id, long match_id, String tmNotes, Hashtable<String, Boolean> boolVals, Hashtable<String, Integer> intVals) {
    		//int auto_score, int tele_score, int other_score, 
    		//int offensive_rating, int defensive_rating){
    	FTSUtilities.printToConsole("TeamMatchDBAdapter::updateTeamMatch\n");
        ContentValues args = new ContentValues();
        args.put(_ID, team_match_id);
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_MATCH_ID, match_id);
        args.put(COLUMN_NAME_TEAM_MATCH_NOTES, tmNotes);
        args.put(COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT, Boolean.TRUE.toString());
        
        Enumeration<String> boolKeys = boolVals.keys();
        while(boolKeys.hasMoreElements()) {
        	String key = boolKeys.nextElement();
        	args.put(key, Boolean.toString(boolVals.get(key)));
        }

        Enumeration<String> intKeys = intVals.keys();
        while(intKeys.hasMoreElements()) {
        	String key = intKeys.nextElement();
        	args.put(key, intVals.get(key));
        }
        
        String WHERE = TeamMatchDBAdapter._ID + "=" + team_match_id;
        
        return this.mDb.update(TABLE_NAME, args, WHERE, null) >0; 
    }
    
    public boolean resetTeamMatchDataExportField(Cursor exportedData) {
    	FTSUtilities.printToConsole("TeamMatchDBAdapter::resetTeamMatchDataExportField\n");
    	
    	long rowID = Long.MIN_VALUE;
    	
    	exportedData.moveToPosition(-1);	// position before the first record
    	
    	while(exportedData.moveToNext()) {
    		rowID = exportedData.getLong(exportedData.getColumnIndex(TeamMatchDBAdapter._ID));
    		
    		ContentValues args = new ContentValues();
            args.put(_ID, rowID);
            args.put(COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT, Boolean.FALSE.toString());
            
            String WHERE = TeamMatchDBAdapter._ID + "=" + rowID;
            
            return this.mDb.update(TABLE_NAME, args, WHERE, null) >0;
    	}
    	
    	
    	return false;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllTeamMatches() {

        return this.mDb.query(TABLE_NAME, new String[] { _ID,
        		COLUMN_NAME_TEAM_ID, COLUMN_NAME_MATCH_ID, COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION,
        		COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA
        		}, null, null, null, null, null);
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries that have data ready to export
     */
    public Cursor getTeamMatchesWithDataToExport() {
        String WHERE = TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT + "=" + Boolean.TRUE.toString();

        return this.mDb.query(TABLE_NAME, this.allColumnNames, WHERE, null, null, null, null);
    }

    /**
     * Set the Data Ready To Export field for a list of elements in the DB
     * @param exportedItems IDs of the entries to set to exported
     * @return A boolean denoting status
     */
    public boolean setItemsExported(ArrayList<Long> exportedItems) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT, Boolean.FALSE.toString());

        String WHERE = "";
        for(int i = 0; i < exportedItems.size(); i++) {
            WHERE += TeamMatchDBAdapter._ID + "=" + String.valueOf(exportedItems.get(i));

            if(i != exportedItems.size() - 1) {
                WHERE += " OR ";
            }
        }

        return this.mDb.update(TABLE_NAME, args, WHERE, null) > 0;
    }

    /**
     * Return a Cursor over the list of all unique matches in the database
     * 
     * @return Cursor over all unique Match numbers
     */
    /**
     * TODO - move this to MatchDataDBAdapter
     */
    public Cursor getAllMatchNumbers() {
    	String SELECT_QUERY = "SELECT DISTINCT t2." + MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER + ", t2." + MatchDataDBAdapter._ID;
    	SELECT_QUERY += " FROM " + TeamMatchDBAdapter.TABLE_NAME + " AS t1";
    	SELECT_QUERY += " INNER JOIN " + MatchDataDBAdapter.TABLE_NAME + " AS t2";
    	SELECT_QUERY += " ON t1." + TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + " = t2." + MatchDataDBAdapter._ID;
    	SELECT_QUERY += " ORDER BY " + MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER + " ASC";
    	return this.mDb.rawQuery(SELECT_QUERY, null);
    }

    /**
     * Return a Cursor over the list of teams for a given match in the database
     * 
     * @return Cursor over all Team IDs for a given match number
     */
    public Cursor getTeamNumbersforMatch(long matchID) {
    	String SELECT_QUERY = "SELECT * FROM " + TeamMatchDBAdapter.TABLE_NAME + " AS t1";
    	SELECT_QUERY += " INNER JOIN " + TeamDataDBAdapter.TABLE_NAME + " AS t2";
    	SELECT_QUERY += " ON t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + " = t2." + TeamDataDBAdapter._ID;
    	SELECT_QUERY += " WHERE t1." + TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + "=" + String.valueOf(matchID);
    	SELECT_QUERY += " ORDER BY t2." + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + " ASC";
    	return this.mDb.rawQuery(SELECT_QUERY, null);
        //return this.mDb.query(TABLE_NAME, new String[] { _ID,
        //		COLUMN_NAME_TEAM_ID}, COLUMN_NAME_MATCH_ID + "=" + String.valueOf(matchID), null, null, null, COLUMN_NAME_TEAM_ID + " ASC");
    }
    
    public Cursor getTeamNumberForMatchAndAlliancePosition(long matchID, String AlliancePos) {
    	String SELECT_QUERY = "SELECT t1." + TeamMatchDBAdapter._ID + ", t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + ", t2." + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER;
    	SELECT_QUERY += " FROM " + TeamMatchDBAdapter.TABLE_NAME + " AS t1";
    	SELECT_QUERY += " INNER JOIN " + TeamDataDBAdapter.TABLE_NAME + " AS t2";
    	SELECT_QUERY += " ON t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + " = t2." + TeamDataDBAdapter._ID;
    	SELECT_QUERY += " WHERE t1." + TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + "=" + matchID;
    	SELECT_QUERY += " AND t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION + "=\"" + AlliancePos + "\"";
    	SELECT_QUERY += " ORDER BY t2." + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + " ASC";
    	return this.mDb.rawQuery(SELECT_QUERY, null);
    }
    
    public Cursor getMatchesForTeam(long teamID) {
    	String selection = COLUMN_NAME_TEAM_ID + "=" + String.valueOf(teamID);
    	Cursor mCursor = this.mDb.query(TABLE_NAME, this.allColumnNames, selection, null, null, null, COLUMN_NAME_MATCH_ID);
    	if(mCursor != null) {
    		mCursor.moveToFirst();
    	}
    	return mCursor;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getTeamMatch(long rowId) throws SQLException {

        Cursor mCursor = this.mDb.query(true, TABLE_NAME, this.allColumnNames,
        		_ID + "=" + rowId, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public long getTeamMatchID(long matchID, long teamID) throws SQLException {

    	String WHERE = TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + "=" + matchID;
    	WHERE += " AND " + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + "=" + teamID;
        Cursor mCursor = this.mDb.query(true, TABLE_NAME, this.allColumnNames, WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor.getLong(mCursor.getColumnIndex(_ID));
    }

    /**
     * Return a Cursor positioned at the entry that matches the given TeamMatchID
     * @param tmID
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getTeamMatch(int tmID) throws SQLException {

        Cursor mCursor = this.mDb.query(true, TABLE_NAME, this.allColumnNames,
        		_ID + "=" + tmID, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }
    
    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteTeamMatch(long rowId) {

        return this.mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }
    
    public void deleteAllData()
    {
        mDb.delete(TABLE_NAME, null, null);
    }

    public boolean populateTestData(long[] matchIDs, long[] teamIDs) {
    	FTSUtilities.printToConsole("TeamMatchDBAdapter::populateTestData\n");
    	deleteAllData();
    	MatchDataDBAdapter mdDBAdapter = new MatchDataDBAdapter(this.mCtx).open();
    	//Set<Integer> teamNums = FTSUtilities.getTestTeamNumbers(); // {1425, 1520, 2929, 1114, 500, 600, 700, 800, 900, 1000};
    	int teamOffset = 0;
    	boolean result = true;
        int lineCount = 0;
    	for(long matchID : matchIDs) {
            lineCount++;
            FTSUtilities.printToConsole("TeamMatchDBAdapter::populateTestData : Creating matchID: " + matchID + "\n");
    		long tempTeamIDs[] = new long[6];
	    	for(int i = 0; i < FTSUtilities.ALLIANCE_POSITION.NOT_SET.allianceIndex(); i++) {
	    		int teamIndex = (i + teamOffset) % teamIDs.length;
	    		tempTeamIDs[i] = teamIDs[teamIndex];
	    		result &= (this.createTeamMatch(FTSUtilities.ALLIANCE_POSITION.getAlliancePositionForIndex(i), teamIDs[teamIndex], matchID) >= 0);
	    	}
	    	if(++teamOffset >= teamIDs.length) {
	    		teamOffset = 0;
	    	}
	    	result &= mdDBAdapter.setTeamIDsForMatchID(matchID, tempTeamIDs);
    	}
    	return result;
    }

	public Hashtable<String, Integer> getTeamAndMatchNumbersForTeamMatchID(long teamMatchID) {
		Hashtable<String, Integer> nums = new Hashtable<String, Integer>();

		nums.put(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER, -1);
		nums.put(MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER, -1);

		String SELECT_QUERY = "SELECT t2." + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + ", t3." + MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER;
		SELECT_QUERY += " FROM " + TeamMatchDBAdapter.TABLE_NAME + " AS t1";
    	SELECT_QUERY += " INNER JOIN " + TeamDataDBAdapter.TABLE_NAME + " AS t2";
    	SELECT_QUERY += " ON t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + " = t2." + TeamDataDBAdapter._ID;
    	SELECT_QUERY += " INNER JOIN " + MatchDataDBAdapter.TABLE_NAME + " AS t3";
    	SELECT_QUERY += " ON t1." + TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + " = t3." + MatchDataDBAdapter._ID;
    	SELECT_QUERY += " WHERE t1." + TeamMatchDBAdapter._ID + "=" + teamMatchID;
    	Cursor c = this.mDb.rawQuery(SELECT_QUERY, null);
		
    	if(c != null) {
    		String names = "";
    		for(String name : c.getColumnNames()) {
    			names += "*" + name + "*-";
    		}
    		
    		FTSUtilities.printToConsole("TeamMatchDBAdapter::getTeamAndMatchNumbersForTeamMatchID : Cursor Size: " + c.getCount() + " Column Names: " + names);
    		
    		c.moveToFirst();
    		
    		try {
    			int teamID = c.getInt(c.getColumnIndexOrThrow(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER));
    			int matchID = c.getInt(c.getColumnIndexOrThrow(MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER));
    		
    			nums.put(TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER, teamID);
    			nums.put(MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER, matchID);
    		}
    		catch (Exception e) {
    			e.printStackTrace();
    		}
    	}
    	
		return nums;
	}

    public Cursor getStartingPositionData(long tmID) throws SQLException{
        String columns[] = {COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X, COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y, COLUMN_NAME_START_LOCATION_ON_FIELD};
        String WHERE = _ID + "=" + tmID;
        Cursor mCursor = this.mDb.query(true, TABLE_NAME, columns,
                WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    public Cursor getAutoModeData(long tmID) throws SQLException{
        String columns[] = {
                COLUMN_NAME_AUTO_MODE_SAVED, COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X, COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y,
                COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X, COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y,
                COLUMN_NAME_AUTO_TOTE_1_LOCATION_X, COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y,
                COLUMN_NAME_AUTO_TOTE_2_LOCATION_X, COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y,
                COLUMN_NAME_AUTO_TOTE_3_LOCATION_X, COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y,
                COLUMN_NAME_AUTO_CAN_1_LOCATION_X, COLUMN_NAME_AUTO_CAN_1_LOCATION_Y,
                COLUMN_NAME_AUTO_CAN_2_LOCATION_X, COLUMN_NAME_AUTO_CAN_2_LOCATION_Y,
                COLUMN_NAME_AUTO_CAN_3_LOCATION_X, COLUMN_NAME_AUTO_CAN_3_LOCATION_Y,
                COLUMN_NAME_AUTO_CAN_4_LOCATION_X, COLUMN_NAME_AUTO_CAN_4_LOCATION_Y,
                COLUMN_NAME_AUTO_CAN_5_LOCATION_X, COLUMN_NAME_AUTO_CAN_5_LOCATION_Y,
                COLUMN_NAME_AUTO_CAN_6_LOCATION_X, COLUMN_NAME_AUTO_CAN_6_LOCATION_Y,
                COLUMN_NAME_AUTO_CAN_7_LOCATION_X, COLUMN_NAME_AUTO_CAN_7_LOCATION_Y,
                COLUMN_NAME_AUTO_ROBOT_VISIBLE, COLUMN_NAME_AUTO_TOTE1_VISIBLE,
                COLUMN_NAME_AUTO_TOTE2_VISIBLE, COLUMN_NAME_AUTO_TOTE3_VISIBLE,
                COLUMN_NAME_AUTO_CAN1_VISIBLE, COLUMN_NAME_AUTO_CAN2_VISIBLE,
                COLUMN_NAME_AUTO_CAN3_VISIBLE, COLUMN_NAME_AUTO_CAN4_VISIBLE,
                COLUMN_NAME_AUTO_CAN5_VISIBLE, COLUMN_NAME_AUTO_CAN6_VISIBLE, COLUMN_NAME_AUTO_CAN7_VISIBLE,
                COLUMN_NAME_AUTO_TOTES_PICKED_UP, COLUMN_NAME_AUTO_TOTES_STACKED, COLUMN_NAME_AUTO_TOTES_SCORED,
                COLUMN_NAME_AUTO_CANS_PICKED_UP, COLUMN_NAME_AUTO_CANS_SCORED, COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP,
                COLUMN_NAME_AUTO_ROBOT_STACK_LIST
        };

        String WHERE = _ID + "=" + tmID;
        Cursor mCursor = this.mDb.query(true, TABLE_NAME, columns,
                WHERE, null, null, null, null, null);

        return mCursor;
    }

    public boolean setStartingPosition(long teamMatchID, int startingPositionX, int startingPositionY, boolean robotOnField) {
        FTSUtilities.printToConsole("TeamMatchDBAdapter::setStartingPosition\n");
        ContentValues args = new ContentValues();
        args.put(_ID, teamMatchID);
        args.put(COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X, startingPositionX);
        args.put(COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y, startingPositionY);
        args.put(COLUMN_NAME_START_LOCATION_ON_FIELD, String.valueOf(robotOnField));

        String WHERE = _ID + "=" + teamMatchID;

        return this.mDb.update(TABLE_NAME, args, WHERE, null) >0;
    }

    /*
    this.teamMatchID, robotFinalLocation.x, robotFinalLocation.y,
                    tote1FinalLocation.x, tote1FinalLocation.y,
                    tote2FinalLocation.x, tote2FinalLocation.y,
                    tote3FinalLocation.x, tote3FinalLocation.y,
                    can1FinalLocation.x, can1FinalLocation.y,
                    can2FinalLocation.x, can2FinalLocation.y,
                    can3FinalLocation.x, can3FinalLocation.y,
                    can4FinalLocation.x, can4FinalLocation.y,
                    can5FinalLocation.x, can5FinalLocation.y,
                    can6FinalLocation.x, can6FinalLocation.y,
                    can7FinalLocation.x, can7FinalLocation.y,
                    this.totesPickedUp, this.totesStacked, this.totesScored,
                    this.cansPickedUp, cansScored, cansGrabbedFromStep);
     */
    public boolean setAutoModeActions(long teamMatchID, int finalAutoModePositionX, int finalAutoModePositionY,
                                      int tote1X, int tote1Y, int tote2X, int tote2Y, int tote3X, int tote3Y,
                                      int can1X, int can1Y, int can2X, int can2Y, int can3X, int can3Y,
                                      int can4X, int can4Y, int can5X, int can5Y, int can6X, long can6Y, int can7X, int can7Y,
                                      boolean robotVis, boolean tote1Vis, boolean tote2Vis, boolean tote3Vis,
                                      boolean can1Vis, boolean can2Vis, boolean can3Vis, boolean can4Vis,
                                      boolean can5Vis, boolean can6Vis, boolean can7Vis,
                                      int totesPickedUp, int totesStacked, int totesScored,
                                      int cansPickedUp, int cansScored, int cansGrabbedFromStep, String stackList) {
        FTSUtilities.printToConsole("TeamMatchDBAdapter::setAutoModeActions\n");
        ContentValues args = new ContentValues();
        args.put(_ID, teamMatchID);
        args.put(COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_MODE_SAVED, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X, finalAutoModePositionX);
        args.put(COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y, finalAutoModePositionY);
        args.put(COLUMN_NAME_AUTO_TOTE_1_LOCATION_X, tote1X);
        args.put(COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y, tote1Y);
        args.put(COLUMN_NAME_AUTO_TOTE_2_LOCATION_X, tote2X);
        args.put(COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y, tote2Y);
        args.put(COLUMN_NAME_AUTO_TOTE_3_LOCATION_X, tote3X);
        args.put(COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y, tote3Y);
        args.put(COLUMN_NAME_AUTO_CAN_1_LOCATION_X, can1X);
        args.put(COLUMN_NAME_AUTO_CAN_1_LOCATION_Y, can1Y);
        args.put(COLUMN_NAME_AUTO_CAN_2_LOCATION_X, can2X);
        args.put(COLUMN_NAME_AUTO_CAN_2_LOCATION_Y, can2Y);
        args.put(COLUMN_NAME_AUTO_CAN_3_LOCATION_X, can3X);
        args.put(COLUMN_NAME_AUTO_CAN_3_LOCATION_Y, can3Y);
        args.put(COLUMN_NAME_AUTO_CAN_4_LOCATION_X, can4X);
        args.put(COLUMN_NAME_AUTO_CAN_4_LOCATION_Y, can4Y);
        args.put(COLUMN_NAME_AUTO_CAN_5_LOCATION_X, can5X);
        args.put(COLUMN_NAME_AUTO_CAN_5_LOCATION_Y, can5Y);
        args.put(COLUMN_NAME_AUTO_CAN_6_LOCATION_X, can6X);
        args.put(COLUMN_NAME_AUTO_CAN_6_LOCATION_Y, can6Y);
        args.put(COLUMN_NAME_AUTO_CAN_7_LOCATION_X, can7X);
        args.put(COLUMN_NAME_AUTO_CAN_7_LOCATION_Y, can7Y);
        args.put(COLUMN_NAME_AUTO_ROBOT_VISIBLE, String.valueOf(robotVis));
        args.put(COLUMN_NAME_AUTO_TOTE1_VISIBLE, String.valueOf(tote1Vis));
        args.put(COLUMN_NAME_AUTO_TOTE2_VISIBLE, String.valueOf(tote2Vis));
        args.put(COLUMN_NAME_AUTO_TOTE3_VISIBLE, String.valueOf(tote3Vis));
        args.put(COLUMN_NAME_AUTO_CAN1_VISIBLE, String.valueOf(can1Vis));
        args.put(COLUMN_NAME_AUTO_CAN2_VISIBLE, String.valueOf(can2Vis));
        args.put(COLUMN_NAME_AUTO_CAN3_VISIBLE, String.valueOf(can3Vis));
        args.put(COLUMN_NAME_AUTO_CAN4_VISIBLE, String.valueOf(can4Vis));
        args.put(COLUMN_NAME_AUTO_CAN5_VISIBLE, String.valueOf(can5Vis));
        args.put(COLUMN_NAME_AUTO_CAN6_VISIBLE, String.valueOf(can6Vis));
        args.put(COLUMN_NAME_AUTO_CAN7_VISIBLE, String.valueOf(can7Vis));
        args.put(COLUMN_NAME_AUTO_TOTES_PICKED_UP, totesPickedUp);
        args.put(COLUMN_NAME_AUTO_CANS_PICKED_UP, cansPickedUp);
        args.put(COLUMN_NAME_AUTO_TOTES_STACKED, totesStacked);
        args.put(COLUMN_NAME_AUTO_TOTES_SCORED, totesScored);
        args.put(COLUMN_NAME_AUTO_CANS_SCORED, cansScored);
        args.put(COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP, cansGrabbedFromStep);
        args.put(COLUMN_NAME_AUTO_ROBOT_STACK_LIST, stackList);

        String WHERE = _ID + "=" + teamMatchID;

        return this.mDb.update(TABLE_NAME, args, WHERE, null) >0;
    }

    public String getTeamAllianceForMatch(long teamMatchID) {
        String columns[] = {
                COLUMN_NAME_TEAM_ID,
                COLUMN_NAME_MATCH_ID,
                COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION
        };
        String WHERE = _ID + "=" + teamMatchID;

        Cursor mCursor = this.mDb.query(true, TABLE_NAME, columns,
                WHERE, null, null, null, null, null);

        String matchAlliance = "";
        while(mCursor.moveToNext()) {
            String alliance = mCursor.getString(mCursor.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION));
            if(alliance.contains("Red")) {
                matchAlliance = "Red";
            } else if(alliance.contains("Blue")) {
                matchAlliance = "Blue";
            }
        }
        return matchAlliance;
    }
}

