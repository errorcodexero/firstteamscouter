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

public class TeamDataDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "team_data";

    // Primary Key comprised of two columns
    public static final String COLUMN_NAME_TEAM_NUMBER = "team_number";
    public static final String COLUMN_NAME_TEAM_SUB_NUMBER = "team_sub_number";


    public static final String COLUMN_NAME_TEAM_NAME = "team_name";
    public static final String COLUMN_NAME_TEAM_LOCATION = "team_location";
    public static final String COLUMN_NAME_TEAM_NUM_MEMBERS = "num_team_members";
    public static final String COLUMN_NAME_TEAM_YEAR_CREATED = "team_creation_year";
    public static final String COLUMN_NAME_TEAM_DATA_UPDATED = "team_data_updated";

    public static final String PRIMARY_KEY = " PRIMARY KEY ( " + TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + ", " + TeamDataDBAdapter.COLUMN_NAME_TEAM_SUB_NUMBER + " )";

    public String[] allColumns = {
            _ID,
            COLUMN_NAME_TEAM_NUMBER,
            COLUMN_NAME_TEAM_SUB_NUMBER,
            COLUMN_NAME_TEAM_NAME,
            COLUMN_NAME_TEAM_LOCATION,
            COLUMN_NAME_TEAM_NUM_MEMBERS,
            COLUMN_NAME_TEAM_YEAR_CREATED,
            COLUMN_NAME_TEAM_DATA_UPDATED
    };

    private DatabaseHelper mDbHelper;
    private SQLiteDatabase mDb;

    private final Context mCtx;

    private static class DatabaseHelper extends SQLiteOpenHelper {

        private static DatabaseHelper mInstance = null;

        private DatabaseHelper(Context context) {
            super(context, DBAdapter.DATABASE_NAME, null, DBAdapter.DATABASE_VERSION);
            FTSUtilities.printToConsole("Constructor::TeamDataDBAdapter::DatabaseHelper");
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
        	FTSUtilities.printToConsole("Creating TeamDataDBAdapter::DatabaseHelper");
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
    public TeamDataDBAdapter(Context ctx) {
    	FTSUtilities.printToConsole("Constructor::TeamDataDBAdapter");
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
    public TeamDataDBAdapter openForWrite() throws SQLException {
    	FTSUtilities.printToConsole("Opening TeamDataDBAdapter Database");
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getWritableDatabase();
        FTSUtilities.printToConsole("TeamDataDBAdapter::openForWrite : DB " + ((mDb == null) ? "IS" : "Is Not") + " null");
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
    public TeamDataDBAdapter openForRead() throws SQLException {
        FTSUtilities.printToConsole("Opening TeamDataDBAdapter Database");
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getReadableDatabase();
        FTSUtilities.printToConsole("TeamDataDBAdapter::openForRead : DB " + ((mDb == null) ? "IS" : "Is Not") + " null");
        return this;
    }

    /**
     * close return type: void
     */
    public void close() {
    	FTSUtilities.printToConsole("Closing TeamDataDBAdapter Database");
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
     * Create a new entry. If the entry is successfully created return the new
     * rowId for that entry, otherwise return a -1 to indicate failure.
     * 
     * @param team_number
     * @param team_name
     * @param team_location
     * @param num_team_members
     * @return rowId or -1 if failed
     */
    public long createTeamDataEntry(int team_number, int team_sub_number, String team_name, String team_location, int num_team_members){
        long retVal = -1;
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_NUMBER, team_number);
        args.put(COLUMN_NAME_TEAM_SUB_NUMBER, team_sub_number);
        args.put(COLUMN_NAME_TEAM_NAME, team_name);
        args.put(COLUMN_NAME_TEAM_LOCATION, team_location);
        args.put(COLUMN_NAME_TEAM_NUM_MEMBERS, num_team_members);
        args.put(COLUMN_NAME_TEAM_DATA_UPDATED, Boolean.TRUE.toString());
        boolean success = this.openForWrite().mDb.insert(TABLE_NAME, null, args) > 0;
        if(success) retVal = team_number;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    public long[] createTeamDataEntry(int team_number, int team_sub_number) {
    	Cursor c = null;
    	long retVal[] = {-1,-1};
    	try {
    		c = this.getTeamDataEntry(team_number, team_sub_number);
    		retVal[0] = c.getLong(c.getColumnIndex(COLUMN_NAME_TEAM_NUMBER));
            retVal[1] = c.getLong(c.getColumnIndex(COLUMN_NAME_TEAM_SUB_NUMBER));
    	}
    	catch(Exception e) {
    		ContentValues args = new ContentValues();
            args.put(COLUMN_NAME_TEAM_NUMBER, team_number);
            args.put(COLUMN_NAME_TEAM_SUB_NUMBER, team_sub_number);
            args.put(COLUMN_NAME_TEAM_DATA_UPDATED, Boolean.TRUE.toString());
            retVal[0] = this.openForWrite().mDb.insert(TABLE_NAME, null, args);
            retVal[1] = team_sub_number;
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
     * @param team_location
     * @param num_team_members
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateTeamDataEntry(int team_number, int team_sub_number, String team_name,
    		String team_location, int num_team_members){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_NUMBER, team_number);
        args.put(COLUMN_NAME_TEAM_SUB_NUMBER, team_sub_number);
        args.put(COLUMN_NAME_TEAM_NAME, team_name);
        args.put(COLUMN_NAME_TEAM_LOCATION, team_location);
        args.put(COLUMN_NAME_TEAM_NUM_MEMBERS, num_team_members);
        args.put(COLUMN_NAME_TEAM_DATA_UPDATED, Boolean.TRUE.toString());
        boolean retVal = this.openForWrite().mDb.update(TABLE_NAME, args,COLUMN_NAME_TEAM_NUMBER + "=" + team_number, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param teamNumber
     * @return true if deleted, false otherwise
     */
    public boolean deleteTeamDataEntry(int teamNumber) {
        boolean retVal = this.openForWrite().mDb.delete(TABLE_NAME, COLUMN_NAME_TEAM_NUMBER + "=" + teamNumber, null) > 0;
        if(!this.dbIsClosed()) this.close();
    	return retVal;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllTeamDataEntries() {
        String SORT = COLUMN_NAME_TEAM_NUMBER + " ASC";
        Cursor mCursor = this.openForRead().mDb.query(TABLE_NAME, allColumns, null, null, null, null, SORT);
        FTSUtilities.printToConsole("TeamDataDBAdapter::getAllTeamDataEntries : Cursor Size : " + mCursor.getCount() + "\n");
        return mCursor;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getUpdatedTeamDataEntries() {
        String WHERE = COLUMN_NAME_TEAM_DATA_UPDATED + "=" + Boolean.TRUE.toString();
        String SORT = COLUMN_NAME_TEAM_NUMBER + " ASC";
        Cursor mCursor = this.openForRead().mDb.query(TABLE_NAME, allColumns, WHERE, null, null, null, SORT);
        FTSUtilities.printToConsole("TeamDataDBAdapter::getAllTeamDataEntries : Cursor Size : " + mCursor.getCount() + "\n");
        return mCursor;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param teamNumber
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public Cursor getTeamDataEntry(long teamNumber, long team_sub_number) throws SQLException {
        String WHERE = COLUMN_NAME_TEAM_NUMBER + "=" + teamNumber;
        Cursor mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns, WHERE, null, null, null, null, null);
        if (mCursor != null) {
            mCursor.moveToFirst();
        }
        return mCursor;
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
    
    public void deleteAllData()
    {
        this.openForWrite().mDb.delete(TABLE_NAME, null, null);
    }
    
    public long[] populateTestData() {
    	FTSUtilities.printToConsole("TeamDataDBAdapter::populateTestData\n");
    	//deleteAllData();
    	
    	Set<Integer> teamNums = FTSUtilities.getTestTeamNumbers();
    	long teamIDs[] = new long[teamNums.size()];
    	int i = 0;
    	
    	for(int teamNum : teamNums) {
    		teamIDs[i++] = this.createTeamDataEntry(teamNum, 0, FTSUtilities.getTeamName(teamNum), "Location", 42);
    	}
    	return teamIDs;
    }
}
