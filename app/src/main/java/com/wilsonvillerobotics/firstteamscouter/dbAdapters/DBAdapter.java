package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import com.wilsonvillerobotics.firstteamscouter.utilities.DataXmlExporter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.DataXmlImporter;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

//import org.jumpmind.symmetric.android.SQLiteOpenHelperRegistry;
//import org.jumpmind.symmetric.android.SymmetricService;
//import org.jumpmind.symmetric.common.ParameterConstants;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

public class DBAdapter {

    public static final String DATABASE_NAME = "FIRSTTeamScouter.sqlite"; //$NON-NLS-1$

    public static final int DATABASE_VERSION = 54;

    private static final int TABLE_NAME        = 0;
    private static final int CREATE_TABLE_SQL  = 1;
    private static final int BACKUP_TABLE_SQL  = 2;
    private static final int DELETE_TABLE_SQL  = 3;
    private static final int RESTORE_TABLE_SQL = 4;
    

    private static final String AUTO_INC_ID = "_id integer primary key autoincrement, ";
    private static final String TEXT_TYPE = " TEXT";
    private static final String INT_TYPE = " INTEGER";
    private static final String NOT_NULL = " NOT NULL";
    private static final String BOOL_TYPE = " TEXT";
    private static final String COMMA_SEP = ",";

    public static enum TABLE_NAMES {
    	COMPETITION_DATA(0, CompetitionDataDBAdapter.TABLE_NAME, CompetitionDataDBAdapter.class.getCanonicalName()),
    	MATCH_DATA(1, MatchDataDBAdapter.TABLE_NAME, MatchDataDBAdapter.class.getCanonicalName()),
    	NOTES_DATA(2, NotesDataDBAdapter.TABLE_NAME, NotesDataDBAdapter.class.getCanonicalName()),
    	PICTURE_DATA(3,PictureDataDBAdapter.TABLE_NAME, PictureDataDBAdapter.class.getCanonicalName()),
    	PIT_DATA(4, PitDataDBAdapter.TABLE_NAME, PitDataDBAdapter.class.getCanonicalName()),
    	ROBOT_DATA(5, RobotDataDBAdapter.TABLE_NAME, RobotDataDBAdapter.class.getCanonicalName()),
    	TEAM_DATA(6, TeamDataDBAdapter.TABLE_NAME, TeamDataDBAdapter.class.getCanonicalName()),
    	TEAM_MATCH_DATA(7, TeamMatchDBAdapter.TABLE_NAME, TeamMatchDBAdapter.class.getCanonicalName()),
        TEAM_MATCH_TRANSACTION_DATA(8, TeamMatchTransactionDataDBAdapter.TABLE_NAME, TeamMatchTransactionDataDBAdapter.class.getCanonicalName());

    	private int index;
        private String className;
        private String tableName;
    	private TABLE_NAMES(int i, String tName, String aClass) {
    		index = i;
            tableName = tName;
            className = aClass;
    	}

    	public int getIndex() {
    		return index;
    	}

        public Class<?> CLASS() {
            Class<?> c = null;
            try {
                c = Class.forName(className);
            } catch (Exception e) {
                //
            }
            return c;
        }

        public String getTableName() {
            return this.tableName;
        }

        public static TABLE_NAMES getTableByTableName(String tableName) {
            for(TABLE_NAMES tn : TABLE_NAMES.values()) {
                if(tn.getTableName().matches(tableName)) return tn;
            }
            return null;
        }
    }

    public static String getFirstColumnName(TABLE_NAMES tName) {
        String col = "";
        switch (tName) {
            case COMPETITION_DATA:
                col = CompetitionDataDBAdapter.allColumns[0];
                break;
            case MATCH_DATA:
                col = MatchDataDBAdapter.allColumns[0];
                break;
            case NOTES_DATA:
                col = NotesDataDBAdapter.allColumns[0];
                break;
            case PICTURE_DATA:
                col = PictureDataDBAdapter.allColumns[0];
                break;
            case PIT_DATA:
                col = PitDataDBAdapter.allColumns[0];
                break;
            case ROBOT_DATA:
                col = RobotDataDBAdapter.allColumns[0];
                break;
            case TEAM_DATA:
                col = TeamDataDBAdapter.allColumns[0];
                break;
            case TEAM_MATCH_DATA:
                col = TeamMatchDBAdapter.allColumns[0];
                break;
            case TEAM_MATCH_TRANSACTION_DATA:
                col = TeamMatchTransactionDataDBAdapter.allColumns[0];
                break;
        }
        return col;
    }

    public static String getLastColumnName(TABLE_NAMES tName) {
        String col = "";
        switch (tName) {
            case COMPETITION_DATA:
                col = CompetitionDataDBAdapter.allColumns[CompetitionDataDBAdapter.allColumns.length - 1];
                break;
            case MATCH_DATA:
                col = MatchDataDBAdapter.allColumns[MatchDataDBAdapter.allColumns.length - 1];
                break;
            case NOTES_DATA:
                col = NotesDataDBAdapter.allColumns[NotesDataDBAdapter.allColumns.length - 1];
                break;
            case PICTURE_DATA:
                col = PictureDataDBAdapter.allColumns[PictureDataDBAdapter.allColumns.length - 1];
                break;
            case PIT_DATA:
                col = PitDataDBAdapter.allColumns[PitDataDBAdapter.allColumns.length - 1];
                break;
            case ROBOT_DATA:
                col = RobotDataDBAdapter.allColumns[RobotDataDBAdapter.allColumns.length - 1];
                break;
            case TEAM_DATA:
                col = TeamDataDBAdapter.allColumns[TeamDataDBAdapter.allColumns.length - 1];
                break;
            case TEAM_MATCH_DATA:
                col = TeamMatchDBAdapter.allColumns[TeamMatchDBAdapter.allColumns.length - 1];
                break;
            case TEAM_MATCH_TRANSACTION_DATA:
                col = TeamMatchTransactionDataDBAdapter.allColumns[TeamMatchTransactionDataDBAdapter.allColumns.length - 1];
                break;
        }
        return col;
    }

    private static final String[][] TABLE_LIST = {
    	{
    		//0
    		//COMPETITION_DATA
            CompetitionDataDBAdapter.TABLE_NAME,
	    	"CREATE TABLE " + CompetitionDataDBAdapter.TABLE_NAME + " (" +
	    	AUTO_INC_ID +
            CompetitionDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
	        CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_NAME + TEXT_TYPE + COMMA_SEP +
	        CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_LOCATION + TEXT_TYPE + COMMA_SEP +
            CompetitionDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
	   	    ");",
            "SELECT * FROM "  + CompetitionDataDBAdapter.TABLE_NAME,
	   	    "DROP TABLE IF EXISTS " + CompetitionDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + CompetitionDataDBAdapter.TABLE_NAME
    	},
   	    
    	{
    		//1
    		//MATCH_DATA
            MatchDataDBAdapter.TABLE_NAME,
			"CREATE TABLE " + MatchDataDBAdapter.TABLE_NAME + " (" +
			AUTO_INC_ID +
            MatchDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
            MatchDataDBAdapter.COLUMN_NAME_COMPETITION_ID + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_TIME + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_TYPE + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_NUMBER + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_LOCATION + TEXT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_ONE_ID + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_TWO_ID + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_RED_TEAM_THREE_ID + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_ONE_ID + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_TWO_ID + INT_TYPE + COMMA_SEP +
	        MatchDataDBAdapter.COLUMN_NAME_BLUE_TEAM_THREE_ID + INT_TYPE + COMMA_SEP +
	        //MatchDataDBAdapter.COLUMN_NAME_MATCH_DATA_UPDATED + BOOL_TYPE + COMMA_SEP +
            MatchDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
			");",

            "SELECT * FROM "  + MatchDataDBAdapter.TABLE_NAME,
			"DROP TABLE IF EXISTS " + MatchDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + MatchDataDBAdapter.TABLE_NAME
    	},

    	{
    		//2
    		//NOTES_DATA
            NotesDataDBAdapter.TABLE_NAME,
    		"CREATE TABLE " + NotesDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID +
            NotesDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
            NotesDataDBAdapter.COLUMN_NAME_OWNER_ID + INT_TYPE + COMMA_SEP +
			NotesDataDBAdapter.COLUMN_NAME_NOTE_TYPE + TEXT_TYPE + COMMA_SEP +
			NotesDataDBAdapter.COLUMN_NAME_NOTE_TEXT + TEXT_TYPE + COMMA_SEP +
            NotesDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
    		");",

            "SELECT * FROM "  + NotesDataDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + NotesDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + NotesDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//3
    		//PICTURE_DATA
            PictureDataDBAdapter.TABLE_NAME,
    		"CREATE TABLE " + PictureDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID +
            PictureDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
            PictureDataDBAdapter.COLUMN_NAME_OWNER_ID + INT_TYPE + COMMA_SEP +
    		PictureDataDBAdapter.COLUMN_NAME_PICTURE_TYPE + TEXT_TYPE + COMMA_SEP +
    		PictureDataDBAdapter.COLUMN_NAME_PICTURE_URI + TEXT_TYPE + COMMA_SEP +
            PictureDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
    		");",

            "SELECT * FROM "  + PictureDataDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + PictureDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + PictureDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//4
    		//PIT_DATA
            PitDataDBAdapter.TABLE_NAME,
    		"CREATE TABLE " + PitDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID +
            PitDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
    		PitDataDBAdapter.COLUMN_NAME_PIT_INFO + TEXT_TYPE + COMMA_SEP +
            PitDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
    		");",

            "SELECT * FROM "  + PitDataDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + PitDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + PitDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//5
    		//ROBOT_DATA
            RobotDataDBAdapter.TABLE_NAME,
    		"CREATE TABLE " + RobotDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID +
            RobotDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_TEAM_ID + INT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_COMPETITION_ID  + INT_TYPE + COMMA_SEP +
    		RobotDataDBAdapter.COLUMN_NAME_DRIVE_TRAIN_TYPE + TEXT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_WHEEL_TYPE + TEXT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_WHEELS + INT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTE_STACKS + INT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_TOTES_PER_STACK + INT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_NUMBER_CANS_AT_ONCE + INT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_GET_STEP_CANS + BOOL_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_PUT_TOTES_ON_STEP + BOOL_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_ROBOT_SOFTWARE_LANGUAGE + TEXT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_TOTE_MANIPULATOR_TYPE + TEXT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_CAN_MANIPULATOR_TYPE + TEXT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_ROBOT_DRIVE_RANGE + TEXT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_COOPERTITION + BOOL_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_ROBOT_STACKS_FROM + TEXT_TYPE + COMMA_SEP +
            RobotDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
    		");",

            "SELECT * FROM "  + RobotDataDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + RobotDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + RobotDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//6
    		//TEAM_DATA
            /*
            CREATE TABLE something (
              column1 INTEGER NOT NULL,
              column2 INTEGER NOT NULL,
              value,
              PRIMARY KEY ( column1, column2)
            );
             */
            TeamDataDBAdapter.TABLE_NAME,
    		"CREATE TABLE " + TeamDataDBAdapter.TABLE_NAME + " (" +
    		AUTO_INC_ID +
            TeamDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + INT_TYPE + NOT_NULL + COMMA_SEP +
            TeamDataDBAdapter.COLUMN_NAME_TEAM_SUB_NUMBER + INT_TYPE + NOT_NULL + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NAME + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_CITY + TEXT_TYPE + COMMA_SEP +
            TeamDataDBAdapter.COLUMN_NAME_TEAM_STATE + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NUM_MEMBERS + INT_TYPE + COMMA_SEP +
            TeamDataDBAdapter.COLUMN_NAME_TEAM_YEAR_CREATED + TEXT_TYPE + COMMA_SEP +
	        //TeamDataDBAdapter.COLUMN_NAME_TEAM_DATA_UPDATED + BOOL_TYPE + COMMA_SEP +
            TeamDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE + //COMMA_SEP +
            //TeamDataDBAdapter.PRIMARY_KEY +
    		");",

            "SELECT * FROM "  + TeamDataDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + TeamDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + TeamDataDBAdapter.TABLE_NAME
    	},
		
    	{
    		//7
    		//TEAM_MATCH_DATA
            TeamMatchDBAdapter.TABLE_NAME,
    		"CREATE TABLE " + TeamMatchDBAdapter.TABLE_NAME + " (" +
                    AUTO_INC_ID +
                    TeamMatchDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_COMPETITION_ID + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION + TEXT_TYPE + COMMA_SEP +
                    //TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_BROKE_DOWN + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_NO_MOVE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_LOST_CONNECTION + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_START_LOCATION + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_START_LOCATION_ON_FIELD + TEXT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_PICKED_UP + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_STACKED + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_SCORED + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_PICKED_UP + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_SCORED + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_MODE_SAVED + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_1_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_2_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE_3_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_1_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_2_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_3_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_4_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_5_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_5_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_6_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_6_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_7_LOCATION_X + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN_7_LOCATION_Y + INT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE1_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE2_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTE3_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN1_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN2_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN3_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN4_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN5_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN6_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_CAN7_VISIBLE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_STACK_LIST + TEXT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_NOTES + TEXT_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TOTE_STACKER + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_CAN_KINGER + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_COOPERATIVE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_NOODLER + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_NI_SAYER + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_INSIDE + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_FORK_LIFT + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_HANDLE_GRABBER + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_DROP_ALOT + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_TOTE_CONTROL_GREAT_CONTROL + BOOL_TYPE + COMMA_SEP +
                    TeamMatchDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
    		");",

            "SELECT * FROM "  + TeamMatchDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + TeamMatchDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + TeamMatchDBAdapter.TABLE_NAME
    	},

        {
            //8
            //TEAM_MATCH_TRANSACTION_DATA
            TeamMatchTransactionDataDBAdapter.TABLE_NAME,
            "CREATE TABLE " + TeamMatchTransactionDataDBAdapter.TABLE_NAME + " (" +
                AUTO_INC_ID +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TEAM_ID + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_MATCH_ID + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TIMESTAMP + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION + TEXT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_PHASE + TEXT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_NAME + TEXT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_X + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_Y + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_NAME + TEXT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_X + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_Y + INT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_TYPES + TEXT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_STATES + TEXT_TYPE + COMMA_SEP +
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
            ");",

            "SELECT * FROM "  + TeamMatchTransactionDataDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + TeamMatchTransactionDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + TeamMatchTransactionDataDBAdapter.TABLE_NAME
        }
    };

    private final Context context; 
    //private DatabaseHelper DBHelper;
    public DatabaseHelper DBHelper;
    protected SQLiteDatabase db;

    /**
     * Constructor
     * @param ctx
     */
    public DBAdapter(Context ctx)
    {
    	FTSUtilities.printToConsole("Constructor::DBAdapter");
        this.context = ctx.getApplicationContext();
        this.DBHelper = DatabaseHelper.getInstance(this.context);

        /*
        final String HELPER_KEY = "FTSHelperKey";

        // Register the database helper, so it can be shared with the SymmetricService
        SQLiteOpenHelperRegistry.register(HELPER_KEY, DBHelper);
        Intent intent = new Intent(ctx, SymmetricService.class);

        // Notify the service of the database helper key
        intent.putExtra(SymmetricService.INTENTKEY_SQLITEOPENHELPER_REGISTRY_KEY, HELPER_KEY);
        intent.putExtra(SymmetricService.INTENTKEY_REGISTRATION_URL, "http://10.0.0.191:32665/sync/fts-master");
        intent.putExtra(SymmetricService.INTENTKEY_EXTERNAL_ID, "Scout1");
        intent.putExtra(SymmetricService.INTENTKEY_NODE_GROUP_ID, "fts-node");
        intent.putExtra(SymmetricService.INTENTKEY_START_IN_BACKGROUND, true);

        // initial load existing notes from the Client to the Server
        Properties properties = new Properties();
        properties.setProperty(ParameterConstants.AUTO_RELOAD_REVERSE_ENABLED, "true");
        intent.putExtra(SymmetricService.INTENTKEY_PROPERTIES, properties);

        ctx.startService(intent);

        String run = (FTSUtilities.isMyServiceRunning(this.context)) ? " IS " : "IS NOT ";
        FTSUtilities.printToConsole("SymmetricDS Service" + run + "running\n");
        */


    }

    private static class DatabaseHelper extends SQLiteOpenHelper 
    {
        Context ctx;
        private static DatabaseHelper mInstance = null;

        private DatabaseHelper(Context context)
        {
            super(context, DATABASE_NAME, null, DATABASE_VERSION);
            FTSUtilities.printToConsole("Constructor::DBAdapter::DatabaseHelper : DB: " + DATABASE_NAME + "    Version: " + DATABASE_VERSION);
            this.ctx = context;
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
        public void onCreate(SQLiteDatabase db) 
        {
        	String query = "";
        	FTSUtilities.printToConsole("Creating tables in DBAdapter::DatabaseHelper");
        	for(TABLE_NAMES table : TABLE_NAMES.values()) {
        		FTSUtilities.printToConsole("Creating table " + table + "\n\n" + query);
        		query = TABLE_LIST[table.getIndex()][CREATE_TABLE_SQL];
        		db.execSQL(query);
        	}

            // ImportTransactions table - special case
            query = "CREATE TABLE " + ImportTransactionsDBAdapter.TABLE_NAME + " (" +
                    AUTO_INC_ID +
                    ImportTransactionsDBAdapter.COLUMN_NAME_IMPORTED_FILE_NAME + TEXT_TYPE +
                    ")";
            db.execSQL(query);

            setSequenceIDs(db);
        }

        @Override
        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) 
        {               
        	String query = "";
        	FTSUtilities.printToConsole("Upgrading tables in DBAdapter::DatabaseHelper -- Old DB Version: " + oldVersion + " - New DB Version: " + newVersion);

            //String seqQuery = "DELETE FROM sqlite_sequence";
            //db.execSQL(seqQuery);

        	for(TABLE_NAMES table : TABLE_NAMES.values()) {
                FTSUtilities.printToConsole("Backing up data from table: " + table + "\n\n");
                query = TABLE_LIST[table.getIndex()][BACKUP_TABLE_SQL];
                /*try {
                    Method m = table.CLASS().getMethod("restoreTableData", ArrayList.class);
                }catch (Exception e) {
                    FTSUtilities.printToConsole("**** restoreTableData not found for: " + table.className);
                }*/
                ArrayList<HashMap<String, String>> data = new ArrayList<HashMap<String, String>>();
                backupTable(db, query, data);
                Cursor c;

                FTSUtilities.printToConsole("Deleting/Re-Creating table: " + table + "\n\n");
        		query = TABLE_LIST[table.getIndex()][DELETE_TABLE_SQL];
        		FTSUtilities.printToConsole("Delete query: " + query + "\n\n");
        		db.execSQL(query);

                query = TABLE_LIST[table.getIndex()][CREATE_TABLE_SQL];
        		FTSUtilities.printToConsole("Create query: " + query + "\n\n");
        		db.execSQL(query);

                FTSUtilities.printToConsole("Restoring data to table: " + table + "\n\n");
                query = TABLE_LIST[table.getIndex()][RESTORE_TABLE_SQL];

                String tableName = TABLE_LIST[table.getIndex()][TABLE_NAME];
                restoreTable(db, query, tableName, data);
        	}

            ArrayList<HashMap<String, String>> data = new ArrayList<HashMap<String, String>>();
            String table = ImportTransactionsDBAdapter.TABLE_NAME;
            try {
                FTSUtilities.printToConsole("Backing up data from table: " + table + "\n\n");
                // Import Transactions - special case
                query = "SELECT * FROM " + ImportTransactionsDBAdapter.TABLE_NAME;

                backupTable(db, query, data);
            } catch (Exception e) {
                e.printStackTrace();
            }

            try {
                FTSUtilities.printToConsole("Deleting/Re-Creating table: " + table + "\n\n");
                query = "DROP TABLE IF EXISTS " + ImportTransactionsDBAdapter.TABLE_NAME;
                db.execSQL(query);
            } catch (Exception e) {
                e.printStackTrace();
            }

            try {
                query = "CREATE TABLE " + ImportTransactionsDBAdapter.TABLE_NAME + " (" +
                        AUTO_INC_ID +
                        ImportTransactionsDBAdapter.COLUMN_NAME_TABLET_ID + INT_TYPE + COMMA_SEP +
                        ImportTransactionsDBAdapter.COLUMN_NAME_IMPORTED_FILE_NAME + TEXT_TYPE + COMMA_SEP +
                        ImportTransactionsDBAdapter.COLUMN_NAME_READY_TO_EXPORT + BOOL_TYPE +
                        ")";
                FTSUtilities.printToConsole("Create query: " + query + "\n\n");
                db.execSQL(query);
            } catch (Exception e) {
                e.printStackTrace();
            }

            try {
                FTSUtilities.printToConsole("Restoring data to table: " + table + "\n\n");
                query = "SELECT * FROM " + ImportTransactionsDBAdapter.TABLE_NAME;
                //data.clear();
                restoreTable(db, query, "import_transactions", data);
            } catch (Exception e) {
                e.printStackTrace();
            }

            setSequenceIDs(db);
        }

        private void setSequenceIDs(SQLiteDatabase db) {
            String seqQuery;
            long seqNum = Long.parseLong(FTSUtilities.getShortDeviceID(ctx), 16);

            // INSERT INTO TABLE_NAME (column1, column2, column3,...columnN)]
            // VALUES (value1, value2, value3,...valueN);

            try {
                //getWritableDatabase();
                Cursor c = db.query(NotesDataDBAdapter.TABLE_NAME, NotesDataDBAdapter.allColumns, null, null, null, null, NotesDataDBAdapter._ID + " DESC");
                if (c == null || c.getCount() == 0 || (c.moveToNext() && c.getLong(c.getColumnIndex(NotesDataDBAdapter._ID)) < seqNum)) {
                    ContentValues values = new ContentValues();
                    values.put(NotesDataDBAdapter._ID, seqNum);
                    long id = db.insert(NotesDataDBAdapter.TABLE_NAME, null, values);
                    if(id != -1) {
                        String WHERE = NotesDataDBAdapter._ID + "=" + String.valueOf(seqNum);
                        db.delete(NotesDataDBAdapter.TABLE_NAME, WHERE, null);
                    }
                }

                c = db.query(PictureDataDBAdapter.TABLE_NAME, PictureDataDBAdapter.allColumns, null, null, null, null, PictureDataDBAdapter._ID + " DESC");
                if (c == null || c.getCount() == 0 || (c.moveToNext() && c.getLong(c.getColumnIndex(PictureDataDBAdapter._ID)) < seqNum)) {
                    ContentValues values = new ContentValues();
                    values.put(PictureDataDBAdapter._ID, seqNum);
                    long id = db.insert(PictureDataDBAdapter.TABLE_NAME, null, values);
                    if(id != -1) {
                        String WHERE = PictureDataDBAdapter._ID + "=" + String.valueOf(seqNum);
                        db.delete(PictureDataDBAdapter.TABLE_NAME, WHERE, null);
                    }
                }

                c = db.query(PitDataDBAdapter.TABLE_NAME, PitDataDBAdapter.allColumns, null, null, null, null, PitDataDBAdapter._ID + " DESC");
                if (c == null || c.getCount() == 0 || (c.moveToNext() && c.getLong(c.getColumnIndex(PitDataDBAdapter._ID)) < seqNum)) {
                    ContentValues values = new ContentValues();
                    values.put(PitDataDBAdapter._ID, seqNum);
                    long id = db.insert(PitDataDBAdapter.TABLE_NAME, null, values);
                    if(id != -1) {
                        String WHERE = PitDataDBAdapter._ID + "=" + String.valueOf(seqNum);
                        db.delete(PitDataDBAdapter.TABLE_NAME, WHERE, null);
                    }
                }

                c = db.query(RobotDataDBAdapter.TABLE_NAME, RobotDataDBAdapter.allColumns, null, null, null, null, RobotDataDBAdapter._ID + " DESC");
                if (c == null || c.getCount() == 0 || (c.moveToNext() && c.getLong(c.getColumnIndex(RobotDataDBAdapter._ID)) < seqNum)) {
                    ContentValues values = new ContentValues();
                    values.put(RobotDataDBAdapter._ID, seqNum);
                    long id = db.insert(RobotDataDBAdapter.TABLE_NAME, null, values);
                    if(id != -1) {
                        String WHERE = RobotDataDBAdapter._ID + "=" + String.valueOf(seqNum);
                        db.delete(RobotDataDBAdapter.TABLE_NAME, WHERE, null);
                    }
                }

                c = db.query(TeamMatchTransactionDataDBAdapter.TABLE_NAME, TeamMatchTransactionDataDBAdapter.allColumns, null, null, null, null, TeamMatchTransactionDataDBAdapter._ID + " DESC");
                if (c == null || c.getCount() == 0 || (c.moveToNext() && c.getLong(c.getColumnIndex(TeamMatchTransactionDataDBAdapter._ID)) < seqNum)) {
                    ContentValues values = new ContentValues();
                    values.put(TeamMatchTransactionDataDBAdapter._ID, seqNum);
                    long id = db.insert(TeamMatchTransactionDataDBAdapter.TABLE_NAME, null, values);
                    if(id != -1) {
                        String WHERE = TeamMatchTransactionDataDBAdapter._ID + "=" + String.valueOf(seqNum);
                        db.delete(TeamMatchTransactionDataDBAdapter.TABLE_NAME, WHERE, null);
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }

            /*
            seqQuery = "UPDATE sqlite_sequence SET seq=" + seqNum + " WHERE name=" + NotesDataDBAdapter.TABLE_NAME;
            db.execSQL(seqQuery);

            seqQuery = "UPDATE sqlite_sequence SET seq=" + seqNum + " WHERE name=" + PictureDataDBAdapter.TABLE_NAME;
            db.execSQL(seqQuery);

            seqQuery = "UPDATE sqlite_sequence SET seq=" + seqNum + " WHERE name=" + PitDataDBAdapter.TABLE_NAME;
            db.execSQL(seqQuery);

            seqQuery = "UPDATE sqlite_sequence SET seq=" + seqNum + " WHERE name=" + RobotDataDBAdapter.TABLE_NAME;
            db.execSQL(seqQuery);

            seqQuery = "UPDATE sqlite_sequence SET seq=" + seqNum + " WHERE name=" + TeamMatchTransactionDataDBAdapter.TABLE_NAME;
            db.execSQL(seqQuery);
            */
        }

        private void restoreTable(SQLiteDatabase db, String query, String table, ArrayList<HashMap<String, String>> data) {
            Cursor c;
            c = db.rawQuery(query, null);
            //TODO Restore the values in 'data' List
            int index;
            for(HashMap<String, String> datum : data) {
                ContentValues initialValues = new ContentValues();
                for(String col : datum.keySet()) {
                    try {
                        index = c.getColumnIndexOrThrow(col);
                        if(index > 0) {
                            initialValues.put(col, datum.get(col));
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
                if(initialValues.size() > 0) {
                    try {
                        db.insert(table, null, initialValues);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }

        private void backupTable(SQLiteDatabase db, String query, ArrayList<HashMap<String, String>> data) {
            try {
                Cursor c = db.rawQuery(query, null);

                while (c.moveToNext()) {
                    HashMap<String, String> datum = new HashMap<String, String>();
                    for (String column : c.getColumnNames()) {
                        datum.put(column, c.getString(c.getColumnIndex(column)));
                    }
                    data.add(datum);
                }
            }catch (Exception e) {
                e.printStackTrace();
            }
        }

        @Override
    	public void onDowngrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        	FTSUtilities.printToConsole("Downgrading tables in DBAdapter::DatabaseHelper -- Old DB Version: " + oldVersion + " - New DB Version: " + newVersion);
            onUpgrade(db, oldVersion, newVersion);
        }
    }

   /**
     * openForWrite the db
     * @return this
     * @throws SQLException
     * return type: DBAdapter
     */
    public DBAdapter openForWrite() throws SQLException
    {
    	FTSUtilities.printToConsole("Opening DBAdapter Database for Write");
        this.db = this.DBHelper.getWritableDatabase();
        return this;
    }

    /**
     * openForWrite the db
     * @return this
     * @throws SQLException
     * return type: DBAdapter
     */
    public DBAdapter openForRead() throws SQLException
    {
        FTSUtilities.printToConsole("Opening DBAdapter Database for Read");
        this.db = this.DBHelper.getReadableDatabase();
        return this;
    }

    /**
     * close the db 
     * return type: void
     */
    public void close() 
    {
        if(this.db != null && this.db.isOpen()) {
            this.DBHelper.close();
        }
        this.db = null;
    }

    public void deleteTableData() {
        String query;
        try {
            this.openForWrite();
            if(this.db != null && this.db.isOpen()) {
                for (TABLE_NAMES table : TABLE_NAMES.values()) {
                    query = TABLE_LIST[table.getIndex()][DELETE_TABLE_SQL];
                    FTSUtilities.printToConsole("Delete table: " + table.name() + "\n\n");
                    this.db.execSQL(query);

                    query = TABLE_LIST[table.getIndex()][CREATE_TABLE_SQL];
                    FTSUtilities.printToConsole("Create table: " + table.name() + "\n\n");
                    db.execSQL(query);
                }
            }
        } catch (Exception e) {
            if(db != null && db.isOpen()) this.close();
            e.printStackTrace();
        } finally {
            this.close();
        }
    }

    public int exportDatabase() {
        int exportCount = 0;

        try {
            this.openForRead();
            DataXmlExporter dataXmlExporter = new DataXmlExporter(this.db);
            exportCount = dataXmlExporter.export(DATABASE_NAME);
        } catch (Exception e) {
            e.printStackTrace();
        }
        if(this.db.isOpen()) this.db.close();
        return exportCount;
    }

    public String getInsertStatementFromXmlTable(File xmlFile, String tableName, String firstColumn, String lastColumn) {
        String insertStatement = "";

        String fileName = xmlFile.getAbsoluteFile().getName();
        ImportTransactionsDBAdapter itDBAdapter = new ImportTransactionsDBAdapter(context);
        if(itDBAdapter.fileHasNotBeenImported(fileName)) {

            DataXmlImporter parser = null;
            try {
                parser = new DataXmlImporter(xmlFile.getCanonicalPath());
            } catch (Exception e) {
                e.printStackTrace();
            }

            insertStatement = parser.parseXML(this.context, tableName, firstColumn, lastColumn);
        }
        return insertStatement;
    }

    public boolean importRecords(String insertStatement) {
        boolean retVal = true;
        if(insertStatement != null && !insertStatement.matches("")) {
            try {
                this.openForWrite().db.execSQL(insertStatement);
            } catch(Exception e) {
                e.printStackTrace();
                retVal = false;
            } finally {
                if(this.db.isOpen()) this.close();
            }
        }
        return true;
    }
}
