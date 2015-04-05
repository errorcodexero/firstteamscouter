package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.provider.BaseColumns;

public class TeamMatchDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME = "team_match";

    // Columns
    public static final String COLUMN_NAME_TEAM_ID = "team_id";
    public static final String COLUMN_NAME_MATCH_ID = "match_id";
    public static final String COLUMN_NAME_COMPETITION_ID = "competition_id";
    public static final String COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION = "alliance_position";
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
    public static final String COLUMN_NAME_AUTO_CAN_1_LOCATION_X  = "auto_can_1_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_1_LOCATION_Y  = "auto_can_1_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_2_LOCATION_X  = "auto_can_2_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_2_LOCATION_Y  = "auto_can_2_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_3_LOCATION_X  = "auto_can_3_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_3_LOCATION_Y  = "auto_can_3_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_4_LOCATION_X  = "auto_can_4_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_4_LOCATION_Y  = "auto_can_4_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_5_LOCATION_X  = "auto_can_5_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_5_LOCATION_Y  = "auto_can_5_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_6_LOCATION_X  = "auto_can_6_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_6_LOCATION_Y  = "auto_can_6_location_Y";
    public static final String COLUMN_NAME_AUTO_CAN_7_LOCATION_X  = "auto_can_7_location_X";
    public static final String COLUMN_NAME_AUTO_CAN_7_LOCATION_Y  = "auto_can_7_location_Y";
    public static final String COLUMN_NAME_AUTO_ROBOT_VISIBLE     = "auto_robot_visible";
    public static final String COLUMN_NAME_AUTO_TOTE1_VISIBLE     = "auto_tote1_visible";
    public static final String COLUMN_NAME_AUTO_TOTE2_VISIBLE     = "auto_tote2_visible";
    public static final String COLUMN_NAME_AUTO_TOTE3_VISIBLE     = "auto_tote3_visible";
    public static final String COLUMN_NAME_AUTO_CAN1_VISIBLE      = "auto_can1_visible";
    public static final String COLUMN_NAME_AUTO_CAN2_VISIBLE      = "auto_can2_visible";
    public static final String COLUMN_NAME_AUTO_CAN3_VISIBLE      = "auto_can3_visible";
    public static final String COLUMN_NAME_AUTO_CAN4_VISIBLE      = "auto_can4_visible";
    public static final String COLUMN_NAME_AUTO_CAN5_VISIBLE      = "auto_can5_visible";
    public static final String COLUMN_NAME_AUTO_CAN6_VISIBLE      = "auto_can6_visible";
    public static final String COLUMN_NAME_AUTO_CAN7_VISIBLE      = "auto_can7_visible";
    public static final String COLUMN_NAME_AUTO_ROBOT_STACK_LIST  = "auto_robot_stack_list";
    public static final String COLUMN_NAME_TOTE_STACKER           = "tote_stacker";
    public static final String COLUMN_NAME_CAN_KINGER             = "can_kinger";
    public static final String COLUMN_NAME_COOPERATIVE            = "cooperative";
    public static final String COLUMN_NAME_NOODLER                = "noodler";
    public static final String COLUMN_NAME_NI_SAYER               = "ni";
    public static final String COLUMN_NAME_TOTE_CONTROL_INSIDE    = "tote_control_inside_robot";
    public static final String COLUMN_NAME_TOTE_CONTROL_FORK_LIFT = "tote_control_fork_lift";
    public static final String COLUMN_NAME_TOTE_CONTROL_HANDLE_GRABBER = "tote_control_handle_grabber";
    public static final String COLUMN_NAME_TOTE_CONTROL_DROP_ALOT = "tote_control_drop_alot";
    public static final String COLUMN_NAME_TOTE_CONTROL_GREAT_CONTROL = "tote_control_great_control";


    // This needs to be moved to the notes_data table
    public static final String COLUMN_NAME_TEAM_MATCH_NOTES = "team_match_notes";

    public static String[] allColumns = new String[]{
    		_ID,
            COLUMN_NAME_TABLET_ID,
    	    COLUMN_NAME_TEAM_ID,
    	    COLUMN_NAME_MATCH_ID,
            COLUMN_NAME_COMPETITION_ID,
    	    COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION,
            COLUMN_NAME_BROKE_DOWN,
            COLUMN_NAME_NO_MOVE,
            COLUMN_NAME_LOST_CONNECTION,
    	    COLUMN_NAME_START_LOCATION,
            COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X,
            COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y,
            COLUMN_NAME_START_LOCATION_ON_FIELD,
            COLUMN_NAME_AUTO_TOTES_PICKED_UP,
            COLUMN_NAME_AUTO_TOTES_STACKED,
            COLUMN_NAME_AUTO_TOTES_SCORED,
            COLUMN_NAME_AUTO_CANS_PICKED_UP,
            COLUMN_NAME_AUTO_CANS_SCORED,
            COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP,
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
    	    COLUMN_NAME_TEAM_MATCH_NOTES,
            COLUMN_NAME_TOTE_STACKER,
            COLUMN_NAME_CAN_KINGER,
            COLUMN_NAME_COOPERATIVE,
            COLUMN_NAME_NOODLER,
            COLUMN_NAME_NI_SAYER,
            COLUMN_NAME_TOTE_CONTROL_INSIDE,
            COLUMN_NAME_TOTE_CONTROL_FORK_LIFT,
            COLUMN_NAME_TOTE_CONTROL_HANDLE_GRABBER,
            COLUMN_NAME_TOTE_CONTROL_DROP_ALOT,
            COLUMN_NAME_TOTE_CONTROL_GREAT_CONTROL,
            COLUMN_NAME_READY_TO_EXPORT
    };

    @Override
    public String[] getAllColumns() {
        return allColumns;
    }

    public static String[] noteFields = {
            COLUMN_NAME_BROKE_DOWN,
            COLUMN_NAME_NO_MOVE,
            COLUMN_NAME_LOST_CONNECTION,
            COLUMN_NAME_TEAM_MATCH_NOTES,
            COLUMN_NAME_TOTE_STACKER,
            COLUMN_NAME_CAN_KINGER,
            COLUMN_NAME_COOPERATIVE,
            COLUMN_NAME_NOODLER,
            COLUMN_NAME_NI_SAYER,
            COLUMN_NAME_TOTE_CONTROL_INSIDE,
            COLUMN_NAME_TOTE_CONTROL_FORK_LIFT,
            COLUMN_NAME_TOTE_CONTROL_HANDLE_GRABBER,
            COLUMN_NAME_TOTE_CONTROL_DROP_ALOT,
            COLUMN_NAME_TOTE_CONTROL_GREAT_CONTROL,
            COLUMN_NAME_READY_TO_EXPORT
    };
    
    /**
     * Constructor - takes the context to allow the database to be
     * opened/created
     * 
     * @param ctx
     *            the Context within which to work
     */
    public TeamMatchDBAdapter(Context ctx) {
        super(ctx);
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
    public TeamMatchDBAdapter openForWrite() throws SQLException {
        return (TeamMatchDBAdapter)openDBForWrite();
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
    public TeamMatchDBAdapter openForRead() throws SQLException {
        return (TeamMatchDBAdapter)openDBForRead();
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * @param alliancePosition
     * @param team_id
     * @param match_id
     * @return rowId or -1 if failed
     */
    public boolean createTeamMatch(long id, long match_id, long comp_id, long team_id, String alliancePosition) {
        ContentValues args = new ContentValues();
        args.put(_ID, id);
        addArgs(args, match_id, comp_id, team_id, alliancePosition);

        long new_id = this.openForWrite().mDb.insert(TABLE_NAME, null, args);
        if(!this.dbIsClosed()) this.close();
        return id == new_id;
    }

    /**
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * @param alliancePosition 
     * @param team_id
     * @param match_id
     * @return rowId or -1 if failed
     */
    public long createTeamMatch(String alliancePosition, long comp_id, long team_id, long match_id) {
        ContentValues args = new ContentValues();
        addArgs(args, match_id, comp_id, team_id, alliancePosition);
        long id = this.openForWrite().mDb.insert(TABLE_NAME, null, args);
        if(!this.dbIsClosed()) this.close();
        return id;
    }

    private void addArgs(ContentValues args, long match_id, long comp_id, long team_id, String alliancePosition) {
        args.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
        args.put(COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION, alliancePosition);
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_MATCH_ID, String.valueOf(match_id));
        args.put(COLUMN_NAME_COMPETITION_ID, comp_id);
        args.put(COLUMN_NAME_AUTO_MODE_SAVED, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_BROKE_DOWN, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_NO_MOVE, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_LOST_CONNECTION, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_START_LOCATION, "none");
        args.put(COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y, 0);
        args.put(COLUMN_NAME_START_LOCATION_ON_FIELD, "none");
        args.put(COLUMN_NAME_AUTO_TOTES_PICKED_UP, 0);
        args.put(COLUMN_NAME_AUTO_TOTES_STACKED, 0);
        args.put(COLUMN_NAME_AUTO_TOTES_SCORED, 0);
        args.put(COLUMN_NAME_AUTO_CANS_PICKED_UP, 0);
        args.put(COLUMN_NAME_AUTO_CANS_SCORED, 0);
        args.put(COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP, 0);
        args.put(COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_TOTE_1_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_TOTE_2_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_TOTE_3_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_CAN_1_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_CAN_1_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_CAN_2_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_CAN_2_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_CAN_3_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_CAN_3_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_CAN_4_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_CAN_4_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_CAN_5_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_CAN_5_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_CAN_6_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_CAN_6_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_CAN_7_LOCATION_X, 0);
        args.put(COLUMN_NAME_AUTO_CAN_7_LOCATION_Y, 0);
        args.put(COLUMN_NAME_AUTO_ROBOT_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_TOTE1_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_TOTE2_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_TOTE3_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_CAN1_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_CAN2_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_CAN3_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_CAN4_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_CAN5_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_CAN6_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_CAN7_VISIBLE, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_ROBOT_STACK_LIST, "none");
        args.put(COLUMN_NAME_TEAM_MATCH_NOTES, "none");
        args.put(COLUMN_NAME_TOTE_STACKER, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_CAN_KINGER, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_COOPERATIVE, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_NOODLER, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_NI_SAYER, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_TOTE_CONTROL_INSIDE, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_TOTE_CONTROL_FORK_LIFT, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_TOTE_CONTROL_HANDLE_GRABBER, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_TOTE_CONTROL_DROP_ALOT, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_TOTE_CONTROL_GREAT_CONTROL, Boolean.FALSE.toString());
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
    }

    /**
     * Update the entry.
     * 
     * @param team_id team ID
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateTeamMatch(long team_match_id, long team_id, long match_id, String tmNotes, Hashtable<String, Boolean> boolVals, Hashtable<String, Integer> intVals) {
    	FTSUtilities.printToConsole("TeamMatchDBAdapter::updateTeamMatch\n");
        ContentValues args = new ContentValues();
        args.put(_ID, team_match_id);
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_MATCH_ID, match_id);
        args.put(COLUMN_NAME_TEAM_MATCH_NOTES, tmNotes);
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
        
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
        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, WHERE, null) >0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }
    
    public boolean resetTeamMatchDataExportField(Cursor exportedData) {
    	FTSUtilities.printToConsole("TeamMatchDBAdapter::resetTeamMatchDataExportField\n");
    	boolean success = true;
    	long rowID = Long.MIN_VALUE;
    	
    	exportedData.moveToPosition(-1);	// position before the first record

        this.openForWrite();
    	while(exportedData.moveToNext()) {
    		rowID = exportedData.getLong(exportedData.getColumnIndex(TeamMatchDBAdapter._ID));

    		ContentValues args = new ContentValues();
            args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.FALSE.toString());
            
            String WHERE = TeamMatchDBAdapter._ID + "=" + rowID;

            success &= this.mDb.update(TABLE_NAME, args, WHERE, null) >0;
    	}
        if(!this.dbIsClosed()) this.close();

    	return success;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    @Override
    public Cursor getAllEntries() {
        return super.getAllEntries(TABLE_NAME, allColumns);
    }

    /**
     * Set the Data Ready To Export field for a list of elements in the DB
     * @param exportedItems IDs of the entries to set to exported
     * @return A boolean denoting status
     */
    public boolean setItemsExported(ArrayList<Long> exportedItems) {
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.FALSE.toString());

        String WHERE = "";
        for(int i = 0; i < exportedItems.size(); i++) {
            WHERE += TeamMatchDBAdapter._ID + "=" + String.valueOf(exportedItems.get(i));

            if(i != exportedItems.size() - 1) {
                WHERE += " OR ";
            }
        }

        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, WHERE, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
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
    	return this.openForRead().mDb.rawQuery(SELECT_QUERY, null);
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
    	return this.openForRead().mDb.rawQuery(SELECT_QUERY, null);
    }
    
    public Cursor getTeamNumberForMatchAndAlliancePosition(long matchID, String AlliancePos) {
    	String SELECT_QUERY = "SELECT t1." + TeamMatchDBAdapter._ID + ", t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + ", t2." + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER;
    	SELECT_QUERY += " FROM " + TeamMatchDBAdapter.TABLE_NAME + " AS t1";
    	SELECT_QUERY += " INNER JOIN " + TeamDataDBAdapter.TABLE_NAME + " AS t2";
    	SELECT_QUERY += " ON t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + " = t2." + TeamDataDBAdapter._ID;
    	SELECT_QUERY += " WHERE t1." + TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + "=" + matchID;
    	SELECT_QUERY += " AND t1." + TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION + "=\"" + AlliancePos + "\"";
    	SELECT_QUERY += " ORDER BY t2." + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + " ASC";
    	return this.openForRead().mDb.rawQuery(SELECT_QUERY, null);
    }
    
    public Cursor getMatchesForTeam(long teamID, long competition_id) {
    	String WHERE = COLUMN_NAME_TEAM_ID + "=" + String.valueOf(teamID);
        WHERE += COLUMN_NAME_COMPETITION_ID + "=" + String.valueOf(competition_id);
    	Cursor mCursor = this.openForRead().mDb.query(TABLE_NAME, this.allColumns, WHERE, null, null, null, COLUMN_NAME_MATCH_ID);
    	if(mCursor != null) {
    		mCursor.moveToFirst();
    	}
    	return mCursor;
    }

    public Cursor getMatchesForCompetition(long compID) throws SQLException {
        String SELECT_QUERY = "SELECT DISTINCT t2." + MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER + ", t2." + MatchDataDBAdapter._ID;
        SELECT_QUERY += " FROM " + TeamMatchDBAdapter.TABLE_NAME + " AS t1";
        SELECT_QUERY += " INNER JOIN " + MatchDataDBAdapter.TABLE_NAME + " AS t2";
        SELECT_QUERY += " ON t1." + TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + " = t2." + MatchDataDBAdapter._ID;
        SELECT_QUERY += " AND t1." + TeamMatchDBAdapter.COLUMN_NAME_COMPETITION_ID + "=" + compID;
        SELECT_QUERY += " ORDER BY " + MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER + " ASC";
        return this.openForRead().mDb.rawQuery(SELECT_QUERY, null);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    @Override
    public Cursor getEntry(long rowId) throws SQLException {
        return super.getEntry(rowId, TABLE_NAME, allColumns);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public long getTeamMatchID(long matchID, long teamID) throws SQLException {

        long id = -1;
    	String WHERE = TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + "=" + matchID;
    	WHERE += " AND " + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + "=" + teamID;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, this.allColumns, WHERE, null, null, null, null, null);
        if (mCursor.moveToFirst()) {
            id = mCursor.getLong(mCursor.getColumnIndex(_ID));
        }

        if(!this.dbIsClosed()) this.close();
        return id;
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
    public boolean deleteAllEntries() {
        return super.deleteAllEntries(TABLE_NAME);
    }

    public void populateTestData() {
        createTeamMatch(1,1,1,1, "Red1");
        createTeamMatch(2, 1, 1, 3, "Red2");
        createTeamMatch(3, 1, 1, 5, "Red3");
        createTeamMatch(4, 1, 1, 2, "Blue1");
        createTeamMatch(5, 1, 1, 4, "Blue2");
        createTeamMatch(6, 1, 1, 6, "Blue3");
        createTeamMatch(7, 2, 1, 10, "Red1");
        createTeamMatch(8, 2, 1, 8, "Red2");
        createTeamMatch(9, 2, 1, 6, "Red3");
        createTeamMatch(10, 2, 1, 9, "Blue1");
        createTeamMatch(11, 2, 1, 7, "Blue2");
        createTeamMatch(12, 2, 1, 5, "Blue3");
        createTeamMatch(13, 3, 1, 1, "Red1");
        createTeamMatch(14, 3, 1, 12, "Red2");
        createTeamMatch(15, 3, 1, 6, "Red3");
        createTeamMatch(16, 3, 1, 2, "Blue1");
        createTeamMatch(17, 3, 1, 11, "Blue2");
        createTeamMatch(18, 3, 1, 5, "Blue3");
        createTeamMatch(19, 4, 1, 1, "Red1");
        createTeamMatch(20, 4, 1, 4, "Red2");
        createTeamMatch(21, 4, 1, 7, "Red3");
        createTeamMatch(22, 4, 1, 6, "Blue1");
        createTeamMatch(23, 4, 1, 9, "Blue2");
        createTeamMatch(24, 4, 1, 12, "Blue3");
        createTeamMatch(25, 5, 1, 10, "Red1");
        createTeamMatch(26, 5, 1, 12, "Red2");
        createTeamMatch(27, 5, 1, 2, "Red3");
        createTeamMatch(28, 5, 1, 4, "Blue1");
        createTeamMatch(29, 5, 1, 11, "Blue2");
        createTeamMatch(30, 5, 1, 7, "Blue3");
        createTeamMatch(31, 6, 2, 4, "Red1");
        createTeamMatch(32, 6, 2, 8, "Red2");
        createTeamMatch(33, 6, 2, 12, "Red3");
        createTeamMatch(34, 6, 2, 3, "Blue1");
        createTeamMatch(35, 6, 2, 6, "Blue2");
        createTeamMatch(36, 6, 2, 9, "Blue3");
        createTeamMatch(37, 7, 2, 2, "Red1");
        createTeamMatch(38, 7, 2, 7, "Red2");
        createTeamMatch(39, 7, 2, 12, "Red3");
        createTeamMatch(40, 7, 2, 3, "Blue1");
        createTeamMatch(41, 7, 2, 5, "Blue2");
        createTeamMatch(42, 7, 2, 11, "Blue3");
    }

    public boolean populateTestData(long competition_id, long[] matchIDs, long[] teamIDs) {
    	FTSUtilities.printToConsole("TeamMatchDBAdapter::populateTestData for competition: " + String.valueOf(competition_id) + "\n");
    	//deleteAllEntries();
    	MatchDataDBAdapter mdDBAdapter = new MatchDataDBAdapter(this.mCtx).openForWrite();
    	//Set<Integer> teamNums = FTSUtilities.getTestTeamNumbers(); // {1425, 1520, 2929, 1114, 500, 600, 700, 800, 900, 1000};
    	int teamOffset = 0;
    	boolean result = true;
        int lineCount = 0;
    	for(long matchID : matchIDs) {
            lineCount++;
            FTSUtilities.printToConsole("TeamMatchDBAdapter::populateTestData : Creating matchID: " + matchID + "\n");
    		long tempTeamIDs[] = new long[6];
	    	for(int i = 0; i < 6; i++) {
	    		int teamIndex = (i + teamOffset) % teamIDs.length;
	    		tempTeamIDs[i] = teamIDs[teamIndex];
	    		result &= (this.createTeamMatch(FTSUtilities.ALLIANCE_POSITION.getAlliancePositionStringForIndex(i), competition_id, teamIDs[teamIndex], matchID) >= 0);
	    	}
	    	if(++teamOffset >= teamIDs.length) {
	    		teamOffset = 0;
	    	}
	    	result &= mdDBAdapter.setTeamIDsForMatchID(competition_id, matchID, tempTeamIDs);
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
    	Cursor c = this.openForRead().mDb.rawQuery(SELECT_QUERY, null);
		
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

        if(c != null && !c.isClosed()) c.close();
        if(!this.dbIsClosed()) this.close();

		return nums;
	}

    public Cursor getStartingPositionData(long tmID) throws SQLException{
        String columns[] = {COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X, COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y, COLUMN_NAME_START_LOCATION_ON_FIELD};
        String WHERE = _ID + "=" + tmID;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, columns,
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
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, columns,
                WHERE, null, null, null, null, null);

        return mCursor;
    }

    public boolean setStartingPosition(long teamMatchID, int startingPositionX, int startingPositionY, boolean robotOnField) {
        FTSUtilities.printToConsole("TeamMatchDBAdapter::setStartingPosition\n");
        ContentValues args = new ContentValues();
        args.put(_ID, teamMatchID);
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
        args.put(COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X, startingPositionX);
        args.put(COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y, startingPositionY);
        args.put(COLUMN_NAME_START_LOCATION_ON_FIELD, String.valueOf(robotOnField));

        String WHERE = _ID + "=" + teamMatchID;

        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, WHERE, null) >0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

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
        args.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
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

        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, WHERE, null) >0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    public boolean updateNotesFields(long teamMatchID, HashMap<String, String> values) {
        ContentValues args = new ContentValues();

        for(String key : values.keySet()) {
            args.put(key, values.get(key));
        }

        String WHERE = _ID + "=" + teamMatchID;

        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args, WHERE, null) >0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    public HashMap<String, String> getNotesData(long teamMatchId) {
        HashMap<String, String> values = new HashMap<String, String>();
        String WHERE = _ID + "=" + teamMatchId;
        Cursor c = this.openForRead().mDb.query(TABLE_NAME, noteFields, WHERE, null, null, null, null);
        if(c != null){
            if(c.moveToNext()) {
                values.put(_ID, String.valueOf(teamMatchId));
                for (String column : noteFields) {
                    values.put(column, c.getString(c.getColumnIndex(column)));
                }
            }
        }
        return values;
    }

    public String getTeamAllianceForMatch(long teamMatchID) {
        String columns[] = {
                COLUMN_NAME_TEAM_ID,
                COLUMN_NAME_MATCH_ID,
                COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION
        };
        String WHERE = _ID + "=" + teamMatchID;

        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, columns,
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

        if(!mCursor.isClosed()) mCursor.close();
        if(!this.dbIsClosed()) this.close();
        return matchAlliance;
    }

    @Override
    public boolean setEntryExported(long rowId) {
        return super.setEntryExported(rowId, TABLE_NAME);
    }

    @Override
    public Cursor getAllEntriesToExport() {
        return super.getAllEntriesToExport(TABLE_NAME, allColumns);
    }
}

