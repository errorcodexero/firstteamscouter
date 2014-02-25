package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.Context;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

public class DBAdapter {

    public static final String DATABASE_NAME = "FIRSTTeamScouter"; //$NON-NLS-1$

    public static final int DATABASE_VERSION = 5;
    
    private static final int CREATE_TABLE_SQL = 0;
    private static final int DELETE_TABLE_SQL = 1;
    

    private static final String AUTO_INC_ID = "_id integer primary key autoincrement, ";
    private static final String TEXT_TYPE = " TEXT";
    private static final String INT_TYPE = " INTEGER";
    private static final String BOOL_TYPE = " TEXT";
    private static final String COMMA_SEP = ",";
    
    private enum TABLE_NAMES {
    	COMPETITION_DATA(0),
    	COMPETITION_MATCHES(1),
    	MATCH_DATA(2),
    	MATCH_NOTES(3),
    	NOTES_DATA(4),
    	PICTURE_DATA(5),
    	PIT_DATA(6),
    	PIT_NOTES(7),
    	PIT_PICTURES(8),
    	ROBOT_DATA(9),
    	ROBOT_NOTES(10),
    	ROBOT_PICTURES(11),
    	TEAM_DATA(12),
    	TEAM_MATCH_DATA(13),
    	TEAM_MATCH_NOTES(14),
    	TEAM_PITS(15),
    	TEAM_ROBOTS(16);
    	
    	private int index;
    	private TABLE_NAMES(int i) {
    		index = i;
    	}
    	public int getIndex() {
    		return index;
    	}
    };
    
    private static final String[][] TABLE_LIST = {
    	{
    		//0
    		//COMPETITION_DATA
	    	"CREATE TABLE " + CompetitionDataDBAdapter.TABLE_NAME + " (" +
	    	AUTO_INC_ID + 
	        CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_ID + INT_TYPE + COMMA_SEP +
	        CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_NAME + TEXT_TYPE + COMMA_SEP +
	        CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_LOCATION + TEXT_TYPE + 
	   	    ");",
	   	    
	   	    "DROP TABLE IF EXISTS " + CompetitionDataDBAdapter.TABLE_NAME
    	},
   	    
    	{
    		//1
    		//COMPETITION_MATCHES
    		"CREATE TABLE " + CompetitionMatchesDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		CompetitionMatchesDBAdapter.COLUMN_NAME_COMPETITION_MATCHES_ID + INT_TYPE + COMMA_SEP +
    		CompetitionMatchesDBAdapter.COLUMN_NAME_COMPETITION_ID + TEXT_TYPE + COMMA_SEP +
    		CompetitionMatchesDBAdapter.COLUMN_NAME_MATCH_ID + TEXT_TYPE +
			");",
			
			"DROP TABLE IF EXISTS " + CompetitionMatchesDBAdapter.TABLE_NAME
    	},
		
    	{
    		//2
    		//MATCH_DATA
			"CREATE TABLE " + MatchDataDBAdapter.TABLE_NAME + " (" +
			AUTO_INC_ID + 
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_DATA_ID + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_LOCATION + TEXT_TYPE + //COMMA_SEP +
	        /*
	        MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_ONE_ID + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_TWO_ID + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_THREE_ID + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_ONE_ID + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_TWO_ID + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_THREE_ID + TEXT_TYPE +
			*/
			");",
			
			"DROP TABLE IF EXISTS " + MatchDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//3
    		//MATCH_NOTES
    		"CREATE TABLE " + MatchNotesDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		MatchNotesDBAdapter.COLUMN_NAME_MATCH_NOTE_ID + INT_TYPE + COMMA_SEP +
    		MatchNotesDBAdapter.COLUMN_NAME_MATCH_ID + TEXT_TYPE + COMMA_SEP +
    		MatchNotesDBAdapter.COLUMN_NAME_NOTE_ID + TEXT_TYPE + 
    		");",
    		
    		"DROP TABLE IF EXISTS " + MatchNotesDBAdapter.TABLE_NAME
    	},
    	
    	{
    		//4
    		//NOTES_DATA
    		"CREATE TABLE " + NotesDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
			NotesDataDBAdapter.COLUMN_NAME_NOTE_ID + INT_TYPE + COMMA_SEP +
			NotesDataDBAdapter.COLUMN_NAME_NOTE_TYPE + TEXT_TYPE + COMMA_SEP +
			NotesDataDBAdapter.COLUMN_NAME_NOTE_TEXT + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + NotesDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//5
    		//PICTURE_DATA
    		"CREATE TABLE " + PictureDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		PictureDataDBAdapter.COLUMN_NAME_PICTURE_ID + INT_TYPE + COMMA_SEP +
    		PictureDataDBAdapter.COLUMN_NAME_PICTURE_TYPE + TEXT_TYPE + COMMA_SEP +
    		PictureDataDBAdapter.COLUMN_NAME_PICTURE_URI + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + PictureDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//6
    		//PIT_DATA
    		"CREATE TABLE " + PitDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		PitDataDBAdapter.COLUMN_NAME_PIT_ID + INT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + PitDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//7
    		//PIT_NOTES
    		"CREATE TABLE " + PitNotesDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		PitNotesDBAdapter.COLUMN_NAME_PIT_NOTE_ID + INT_TYPE + COMMA_SEP +
    		PitNotesDBAdapter.COLUMN_NAME_PIT_ID + TEXT_TYPE + COMMA_SEP +
    		PitNotesDBAdapter.COLUMN_NAME_NOTE_ID + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + PitNotesDBAdapter.TABLE_NAME
    	},
		
    	{
    		//8
    		//PIT_PICTURES
    		"CREATE TABLE " + PitPicturesDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		PitPicturesDBAdapter.COLUMN_NAME_PIT_PICTURE_ID + INT_TYPE + COMMA_SEP +
    		PitPicturesDBAdapter.COLUMN_NAME_PIT_ID + TEXT_TYPE + COMMA_SEP +
    		PitPicturesDBAdapter.COLUMN_NAME_PICTURE_ID + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + PitPicturesDBAdapter.TABLE_NAME
    	},
		
    	{
    		//9
    		//ROBOT_DATA
    		"CREATE TABLE " + RobotDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		RobotDataDBAdapter.COLUMN_NAME_ROBOT_DATA_ID + INT_TYPE + COMMA_SEP +
    		RobotDataDBAdapter.COLUMN_NAME_ROBOT_ID + TEXT_TYPE + COMMA_SEP +
    		RobotDataDBAdapter.COLUMN_NAME_DRIVE_TRAIN_TYPE + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + RobotDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//10
    		//ROBOT_NOTES
    		"CREATE TABLE " + RobotNotesDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		RobotNotesDBAdapter.COLUMN_NAME_ROBOT_NOTE_ID + INT_TYPE + COMMA_SEP +
    		RobotNotesDBAdapter.COLUMN_NAME_ROBOT_ID + TEXT_TYPE + COMMA_SEP +
    		RobotNotesDBAdapter.COLUMN_NAME_NOTE_ID + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + RobotNotesDBAdapter.TABLE_NAME
    	},
		
    	{
    		//11
    		//ROBOT_PICTURES
    		"CREATE TABLE " + RobotPicturesDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		RobotPicturesDBAdapter.COLUMN_NAME_ROBOT_PICTURE_ID + INT_TYPE + COMMA_SEP +
    		RobotPicturesDBAdapter.COLUMN_NAME_ROBOT_ID + TEXT_TYPE + COMMA_SEP +
    		RobotPicturesDBAdapter.COLUMN_NAME_PICTURE_ID + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + RobotPicturesDBAdapter.TABLE_NAME
    	},
		
    	{
    		//12
    		//TEAM_DATA
    		"CREATE TABLE " + TeamDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		TeamDataDBAdapter.COLUMN_NAME_TEAM_ID + INT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NAME + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_LOCATION + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NUM_MEMBERS + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + TeamDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//13
    		//TEAM_MATCH_DATA
    		"CREATE TABLE " + TeamMatchDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_ID + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + TEXT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + TEXT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_MATCH_DATA_SAVED + BOOL_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_SCORE + TEXT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_HI_SCORE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_LO_SCORE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_HI_MISS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_LO_MISS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_HI_HOT + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_LO_HOT + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_COLLECT + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_DEFEND + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_AUTO_MOVE + BOOL_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TELE_SCORE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TELE_HI_SCORE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TELE_LO_SCORE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TELE_HI_MISS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TELE_LO_MISS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TRUSS_TOSS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TRUSS_MISS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TOSS_CATCH + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TOSS_MISS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_ASSIST_RED + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_ASSIST_WHITE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_ASSIST_BLUE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_PASS_SUCCESS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_PASS_MISS + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_DEFEND_RED + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_DEFEND_WHITE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_DEFEND_BLUE + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_DEFEND_GOAL + INT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + TeamMatchDBAdapter.TABLE_NAME
    	},
		
    	{
    		//14
    		//TEAM_MATCH_NOTES
    		"CREATE TABLE " + TeamMatchNotesDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		TeamMatchNotesDBAdapter.COLUMN_NAME_TEAM_MATCH_NOTE_ID + INT_TYPE + COMMA_SEP +
    		TeamMatchNotesDBAdapter.COLUMN_NAME_TEAM_ID + INT_TYPE + COMMA_SEP +
    		TeamMatchNotesDBAdapter.COLUMN_NAME_MATCH_ID + INT_TYPE + COMMA_SEP +
    		TeamMatchNotesDBAdapter.COLUMN_NAME_NOTE_ID + INT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + TeamMatchNotesDBAdapter.TABLE_NAME
    	},
		
    	{
    		//15
    		//TEAM_PITS
    		"CREATE TABLE " + TeamPitsDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		TeamPitsDBAdapter.COLUMN_NAME_TEAM_PIT_ID + INT_TYPE + COMMA_SEP +
			TeamPitsDBAdapter.COLUMN_NAME_TEAM_ID + TEXT_TYPE + COMMA_SEP +
			TeamPitsDBAdapter.COLUMN_NAME_PIT_ID + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + TeamPitsDBAdapter.TABLE_NAME
    	},
		
    	{
    		//16
    		//TEAM_ROBOTS
    		"CREATE TABLE " + TeamRobotsDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID + 
    		TeamRobotsDBAdapter.COLUMN_NAME_TEAM_ROBOTS_ID + INT_TYPE + COMMA_SEP +
			TeamRobotsDBAdapter.COLUMN_NAME_TEAM_ID + TEXT_TYPE + COMMA_SEP +
			TeamRobotsDBAdapter.COLUMN_NAME_ROBOT_ID + TEXT_TYPE +
    		");",
    		
    		"DROP TABLE IF EXISTS " + TeamRobotsDBAdapter.TABLE_NAME
    	}
    };

    private final Context context; 
    //private DatabaseHelper DBHelper;
    public DatabaseHelper DBHelper;
    private SQLiteDatabase db;

    /**
     * Constructor
     * @param ctx
     */
    public DBAdapter(Context ctx)
    {
    	FTSUtilities.printToConsole("Constructor::DBAdapter");
        this.context = ctx;
        this.DBHelper = new DatabaseHelper(this.context);
    }

    private static class DatabaseHelper extends SQLiteOpenHelper 
    {
        DatabaseHelper(Context context) 
        {
            super(context, DATABASE_NAME, null, DATABASE_VERSION);
            FTSUtilities.printToConsole("Constructor::DBAdapter::DatabaseHelper : DB: " + DATABASE_NAME + "    Version: " + DATABASE_VERSION);
        }

        @Override
        public void onCreate(SQLiteDatabase db) 
        {
        	String query = "";
        	FTSUtilities.printToConsole("Creating tables in DBAdapter::DatabaseHelper");
        	for(TABLE_NAMES table : TABLE_NAMES.values()) {
        		FTSUtilities.printToConsole("Creating table " + table + "\n\n" + query);
        		query = TABLE_LIST[table.getIndex()][CREATE_TABLE_SQL];
        		db.execSQL(query);
        	}
        }

        @Override
        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) 
        {               
        	String query = "";
        	FTSUtilities.printToConsole("Upgrading tables in DBAdapter::DatabaseHelper -- Old DB Version: " + oldVersion + " - New DB Version: " + newVersion);
        	for(TABLE_NAMES table : TABLE_NAMES.values()) {
        		FTSUtilities.printToConsole("Deleting/Re-Creating table " + table + "\n\n");
        		query = TABLE_LIST[table.getIndex()][DELETE_TABLE_SQL];
        		FTSUtilities.printToConsole("Delete query: " + query + "\n\n");
        		db.execSQL(query);
        		query = TABLE_LIST[table.getIndex()][CREATE_TABLE_SQL];
        		FTSUtilities.printToConsole("Create query: " + query + "\n\n");
        		db.execSQL(query);
        	}
        }
        
        @Override
    	public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        	FTSUtilities.printToConsole("Downgrading tables in DBAdapter::DatabaseHelper -- Old DB Version: " + oldVersion + " - New DB Version: " + newVersion);
            onUpgrade(db, oldVersion, newVersion);
        }
    } 

   /**
     * open the db
     * @return this
     * @throws SQLException
     * return type: DBAdapter
     */
    public DBAdapter open() throws SQLException 
    {
    	FTSUtilities.printToConsole("Opening DBAdapter Database");
        this.db = this.DBHelper.getWritableDatabase();
        return this;
    }

    /**
     * close the db 
     * return type: void
     */
    public void close() 
    {
        this.DBHelper.close();
    }
}