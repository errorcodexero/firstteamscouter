package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

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
    public static final String COLUMN_NAME_TEAM_MATCH_ID = "team_match_id";
    public static final String COLUMN_NAME_TEAM_ID = "team_id";
    public static final String COLUMN_NAME_MATCH_ID = "match_id";
    public static final String COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION = "alliance_position";
    public static final String COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT = "data_ready_to_export";
    public static final String COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA = "team_match_has_saved_data";
    public static final String COLUMN_NAME_AUTO_SCORE = "auto_score";
    public static final String COLUMN_NAME_AUTO_HI_SCORE = "auto_hi_score";
    public static final String COLUMN_NAME_AUTO_LO_SCORE = "auto_lo_score";
    public static final String COLUMN_NAME_AUTO_HI_MISS = "auto_hi_miss";
    public static final String COLUMN_NAME_AUTO_LO_MISS = "auto_lo_miss";
    public static final String COLUMN_NAME_AUTO_HI_HOT = "auto_hi_hot";
    public static final String COLUMN_NAME_AUTO_LO_HOT = "auto_lo_hot";
    public static final String COLUMN_NAME_AUTO_COLLECT = "auto_collect";
    public static final String COLUMN_NAME_AUTO_DEFEND = "auto_defend";
    public static final String COLUMN_NAME_AUTO_MOVE = "auto_move";
    public static final String COLUMN_NAME_TELE_SCORE = "tele_score";
    public static final String COLUMN_NAME_TELE_HI_SCORE = "tele_hi_score";
    public static final String COLUMN_NAME_TELE_LO_SCORE = "tele_lo_score";
    public static final String COLUMN_NAME_TELE_HI_MISS = "tele_hi_miss";
    public static final String COLUMN_NAME_TELE_LO_MISS = "tele_lo_miss";
    public static final String COLUMN_NAME_TRUSS_TOSS = "truss_toss";
    public static final String COLUMN_NAME_TRUSS_MISS = "truss_miss";
    public static final String COLUMN_NAME_TOSS_CATCH = "toss_catch";
    public static final String COLUMN_NAME_TOSS_MISS = "toss_miss";
    public static final String COLUMN_NAME_ASSIST_RED = "assist_red";
    public static final String COLUMN_NAME_ASSIST_WHITE = "assist_white";
    public static final String COLUMN_NAME_ASSIST_BLUE = "assist_blue";
    public static final String COLUMN_NAME_SHORT_PASS_SUCCESS = "short_pass_success";
    public static final String COLUMN_NAME_SHORT_PASS_MISS = "short_pass_miss";
    public static final String COLUMN_NAME_LONG_PASS_SUCCESS = "long_pass_success";
    public static final String COLUMN_NAME_LONG_PASS_MISS = "long_pass_miss";
    public static final String COLUMN_NAME_DEFEND_RED = "defend_red";
    public static final String COLUMN_NAME_DEFEND_WHITE = "defend_white";
    public static final String COLUMN_NAME_DEFEND_BLUE = "defend_blue";
    public static final String COLUMN_NAME_DEFEND_GOAL = "defend_goal";
    public static final String COLUMN_NAME_BROKE_DOWN = "broke_down";
    public static final String COLUMN_NAME_NO_MOVE = "no_move";
    public static final String COLUMN_NAME_LOST_CONNECTION = "lost_connection";
    public static final String COLUMN_NAME_ROLE_SHOOTER = "role_shooter";
    public static final String COLUMN_NAME_ROLE_DEFENDER = "role_defender";
    public static final String COLUMN_NAME_ROLE_PASSER = "role_passer";
    public static final String COLUMN_NAME_ROLE_CATCHER = "role_catcher";
    public static final String COLUMN_NAME_ROLE_GOALIE = "role_goalie";
    public static final String COLUMN_NAME_START_LOCATION = "starting_location";
    public static final String COLUMN_NAME_START_LOCATION_X = "starting_location_X";
    public static final String COLUMN_NAME_START_LOCATION_Y = "starting_location_Y";
    public static final String COLUMN_NAME_START_LOCATION_ON_FIELD = "starting_location_on_field";
    public static final String COLUMN_NAME_AUTO_TOTES_PICKED_UP = "auto_totes_picked_up";
    public static final String COLUMN_NAME_AUTO_TOTES_STACKED = "auto_totes_stacked";
    public static final String COLUMN_NAME_AUTO_TOTES_SCORED = "auto_totes_scored";
    public static final String COLUMN_NAME_AUTO_CANS_PICKED_UP = "auto_cans_picked_up";
    public static final String COLUMN_NAME_AUTO_CANS_SCORED = "auto_cans_scored";
    public static final String COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP = "auto_cans_pulled_from_step";
    public static final String COLUMN_NAME_AUTO_FINAL_LOCATION_X = "auto_final_location_X";
    public static final String COLUMN_NAME_AUTO_FINAL_LOCATION_Y = "auto_final_location_Y";


    // These should be part of the robot data, not the match data
    public static final String COLUMN_NAME_BALL_CONTROL_GROUND_PICKUP = "ball_control_ground_pickup";
    public static final String COLUMN_NAME_BALL_CONTROL_HUMAN_LOAD = "ball_control_human_load";
    public static final String COLUMN_NAME_BALL_CONTROL_HI_TO_LO = "ball_control_hi_to_lo";
    public static final String COLUMN_NAME_BALL_CONTROL_LO_TO_HI = "ball_control_lo_to_hi";
    public static final String COLUMN_NAME_BALL_CONTROL_HI_TO_HI = "ball_control_hi_to_hi";
    public static final String COLUMN_NAME_BALL_CONTROL_LO_TO_LO = "ball_control_lo_to_lo";
    
    // This needs to be moved to the team_match_notes_data
    public static final String COLUMN_NAME_TEAM_MATCH_NOTES = "team_match_notes";


    private String[] allColumnNames = new String[]{
    		_ID,
    		COLUMN_NAME_TEAM_MATCH_ID,
    	    COLUMN_NAME_TEAM_ID,
    	    COLUMN_NAME_MATCH_ID,
    	    COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION,
    	    COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT,
    	    COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA,
    	    COLUMN_NAME_AUTO_SCORE,
    	    COLUMN_NAME_AUTO_HI_SCORE,
    	    COLUMN_NAME_AUTO_LO_SCORE,
    	    COLUMN_NAME_AUTO_HI_MISS,
    	    COLUMN_NAME_AUTO_LO_MISS,
    	    COLUMN_NAME_AUTO_HI_HOT,
    	    COLUMN_NAME_AUTO_LO_HOT,
    	    COLUMN_NAME_AUTO_COLLECT,
    	    COLUMN_NAME_AUTO_DEFEND,
    	    COLUMN_NAME_AUTO_MOVE,
    	    COLUMN_NAME_TELE_SCORE,
    	    COLUMN_NAME_TELE_HI_SCORE,
    	    COLUMN_NAME_TELE_LO_SCORE,
    	    COLUMN_NAME_TELE_HI_MISS,
    	    COLUMN_NAME_TELE_LO_MISS,
    	    COLUMN_NAME_TRUSS_TOSS,
    	    COLUMN_NAME_TRUSS_MISS,
    	    COLUMN_NAME_TOSS_CATCH,
    	    COLUMN_NAME_TOSS_MISS,
    	    COLUMN_NAME_ASSIST_RED,
    	    COLUMN_NAME_ASSIST_WHITE,
    	    COLUMN_NAME_ASSIST_BLUE,
    	    COLUMN_NAME_SHORT_PASS_SUCCESS,
    	    COLUMN_NAME_SHORT_PASS_MISS,
    	    COLUMN_NAME_LONG_PASS_SUCCESS,
    	    COLUMN_NAME_LONG_PASS_MISS,
    	    COLUMN_NAME_DEFEND_RED,
    	    COLUMN_NAME_DEFEND_WHITE,
    	    COLUMN_NAME_DEFEND_BLUE,
    	    COLUMN_NAME_DEFEND_GOAL,
    	    COLUMN_NAME_BROKE_DOWN,
    	    COLUMN_NAME_NO_MOVE,
    	    COLUMN_NAME_LOST_CONNECTION,
    	    COLUMN_NAME_ROLE_SHOOTER,
    	    COLUMN_NAME_ROLE_DEFENDER,
    	    COLUMN_NAME_ROLE_PASSER,
    	    COLUMN_NAME_ROLE_CATCHER,
    	    COLUMN_NAME_ROLE_GOALIE,
    	    COLUMN_NAME_START_LOCATION,
            COLUMN_NAME_START_LOCATION_X,
            COLUMN_NAME_START_LOCATION_Y,
            COLUMN_NAME_AUTO_FINAL_LOCATION_X,
            COLUMN_NAME_AUTO_FINAL_LOCATION_Y,
            COLUMN_NAME_AUTO_TOTES_PICKED_UP,
            COLUMN_NAME_AUTO_TOTES_STACKED,
            COLUMN_NAME_AUTO_TOTES_SCORED,
            COLUMN_NAME_AUTO_CANS_PICKED_UP,
            COLUMN_NAME_AUTO_CANS_SCORED,
            COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP,
            COLUMN_NAME_START_LOCATION_ON_FIELD,
    	    COLUMN_NAME_BALL_CONTROL_GROUND_PICKUP,
    	    COLUMN_NAME_BALL_CONTROL_HUMAN_LOAD,
    	    COLUMN_NAME_BALL_CONTROL_HI_TO_LO,
    	    COLUMN_NAME_BALL_CONTROL_LO_TO_HI,
    	    COLUMN_NAME_BALL_CONTROL_HI_TO_HI,
    	    COLUMN_NAME_BALL_CONTROL_LO_TO_LO,
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
        		COLUMN_NAME_AUTO_SCORE, COLUMN_NAME_TELE_SCORE, COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA
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
    	for(long matchID : matchIDs) {
    		FTSUtilities.printToConsole("TeamMatchDBAdapter::populateTestData : Creating matchID: " + matchID + "\n");
    		long tempTeamIDs[] = new long[6];
	    	for(int i = 0; i < FTSUtilities.ALLIANCE_POSITION.NOT_SET.allianceID(); i++) {
	    		int teamIndex = (i + teamOffset) % teamIDs.length;
	    		tempTeamIDs[i] = teamIDs[teamIndex];
	    		result &= (this.createTeamMatch(FTSUtilities.ALLIANCE_POSITION.getAlliancePositionForID(i), teamIDs[teamIndex], matchID) >= 0);
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

    public Cursor getStartingPosition(long tmID) throws SQLException{
        String columns[] = {COLUMN_NAME_START_LOCATION_X, COLUMN_NAME_START_LOCATION_Y, COLUMN_NAME_START_LOCATION_ON_FIELD};
        Cursor mCursor = this.mDb.query(true, TABLE_NAME, columns,
                _ID + "=" + tmID, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
    }

    public boolean setStartingPosition(long teamMatchID, int startingPositionX, int startingPositionY, boolean robotOnField) {
        FTSUtilities.printToConsole("TeamMatchDBAdapter::setStartingPosition\n");
        ContentValues args = new ContentValues();
        args.put(_ID, teamMatchID);
        args.put(COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_START_LOCATION_X, startingPositionX);
        args.put(COLUMN_NAME_START_LOCATION_Y, startingPositionY);
        args.put(COLUMN_NAME_START_LOCATION_ON_FIELD, String.valueOf(robotOnField));

        String WHERE = TeamMatchDBAdapter._ID + "=" + teamMatchID;

        return this.mDb.update(TABLE_NAME, args, WHERE, null) >0;
    }

    public boolean setAutoModeActions(long teamMatchID, int finalAutoModePositionX, int finalAutoModePositionY,
                                      int totesPickedUp, int cansPickedUp, int totesStacked, int totesScored,
                                      int cansScored, int cansGrabbedFromStep) {
        FTSUtilities.printToConsole("TeamMatchDBAdapter::setAutoModeActions\n");
        ContentValues args = new ContentValues();
        args.put(_ID, teamMatchID);
        args.put(COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_FINAL_LOCATION_X, finalAutoModePositionX);
        args.put(COLUMN_NAME_AUTO_FINAL_LOCATION_Y, finalAutoModePositionY);
        args.put(COLUMN_NAME_AUTO_TOTES_PICKED_UP, totesPickedUp);
        args.put(COLUMN_NAME_AUTO_CANS_PICKED_UP, cansPickedUp);
        args.put(COLUMN_NAME_AUTO_TOTES_STACKED, totesStacked);
        args.put(COLUMN_NAME_AUTO_TOTES_SCORED, totesScored);
        args.put(COLUMN_NAME_AUTO_CANS_SCORED, cansScored);
        args.put(COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP, cansGrabbedFromStep);

        String WHERE = TeamMatchDBAdapter._ID + "=" + teamMatchID;

        return this.mDb.update(TABLE_NAME, args, WHERE, null) >0;
    }
}
