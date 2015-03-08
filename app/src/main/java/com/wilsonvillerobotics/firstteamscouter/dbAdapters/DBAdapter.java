package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import org.jumpmind.symmetric.android.SQLiteOpenHelperRegistry;
import org.jumpmind.symmetric.android.SymmetricService;
import org.jumpmind.symmetric.common.ParameterConstants;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Properties;

public class DBAdapter {

    public static final String DATABASE_NAME = "FIRSTTeamScouter.sqlite"; //$NON-NLS-1$

    public static final int DATABASE_VERSION = 2;

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

    private enum TABLE_NAMES {
    	COMPETITION_DATA(0, CompetitionDataDBAdapter.class.getCanonicalName()),
    	MATCH_DATA(1, MatchDataDBAdapter.class.getCanonicalName()),
    	NOTES_DATA(2, NotesDataDBAdapter.class.getCanonicalName()),
    	PICTURE_DATA(3, PictureDataDBAdapter.class.getCanonicalName()),
    	PIT_DATA(4, PitDataDBAdapter.class.getCanonicalName()),
    	ROBOT_DATA(5, RobotDataDBAdapter.class.getCanonicalName()),
    	TEAM_DATA(6, TeamDataDBAdapter.class.getCanonicalName()),
    	TEAM_MATCH_DATA(7, TeamMatchDBAdapter.class.getCanonicalName()),
        TEAM_MATCH_TRANSACTION_DATA(8, TeamMatchTransactionDataDBAdapter.class.getCanonicalName()),
        TEAM_MATCH_TRANSACTIONS(9, TeamMatchTransactionsDBAdapter.class.getCanonicalName());
    	
    	private int index;
        private String className;
    	private TABLE_NAMES(int i, String aClass) {
    		index = i;
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
    };

    private static final String[][] TABLE_LIST = {
    	{
    		//0
    		//COMPETITION_DATA
            CompetitionDataDBAdapter.TABLE_NAME,
	    	"CREATE TABLE " + CompetitionDataDBAdapter.TABLE_NAME + " (" +
	    	AUTO_INC_ID + 
	        CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_NAME + TEXT_TYPE + COMMA_SEP +
	        CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_LOCATION + TEXT_TYPE + 
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
	        MatchDataDBAdapter.COLUMN_NAME_MATCH_DATA_UPDATED + BOOL_TYPE +
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
            NotesDataDBAdapter.COLUMN_NAME_OWNER_ID + INT_TYPE + COMMA_SEP +
			NotesDataDBAdapter.COLUMN_NAME_NOTE_TYPE + TEXT_TYPE + COMMA_SEP +
			NotesDataDBAdapter.COLUMN_NAME_NOTE_TEXT + TEXT_TYPE +
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
            PictureDataDBAdapter.COLUMN_NAME_OWNER_ID + INT_TYPE + COMMA_SEP +
    		PictureDataDBAdapter.COLUMN_NAME_PICTURE_TYPE + TEXT_TYPE + COMMA_SEP +
    		PictureDataDBAdapter.COLUMN_NAME_PICTURE_URI + TEXT_TYPE +
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
    		PitDataDBAdapter.COLUMN_NAME_PIT_INFO + TEXT_TYPE +
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
            RobotDataDBAdapter.COLUMN_NAME_ROBOT_STACKS_FROM + TEXT_TYPE +
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
    		//AUTO_INC_ID +
            "_id integer, " +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NUMBER + INT_TYPE + NOT_NULL + COMMA_SEP +
            TeamDataDBAdapter.COLUMN_NAME_TEAM_SUB_NUMBER + INT_TYPE + NOT_NULL + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NAME + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_LOCATION + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_NUM_MEMBERS + INT_TYPE + COMMA_SEP +
            TeamDataDBAdapter.COLUMN_NAME_TEAM_YEAR_CREATED + TEXT_TYPE + COMMA_SEP +
	        TeamDataDBAdapter.COLUMN_NAME_TEAM_DATA_UPDATED + BOOL_TYPE + COMMA_SEP +
            TeamDataDBAdapter.PRIMARY_KEY +
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
    		TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_COMPETITION_ID + INT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION + TEXT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA + BOOL_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT + BOOL_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_BROKE_DOWN + BOOL_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_NO_MOVE + BOOL_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_LOST_CONNECTION + BOOL_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_START_LOCATION + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_PICKED_UP + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_STACKED + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_TOTES_SCORED + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_PICKED_UP + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_SCORED + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP + INT_TYPE + COMMA_SEP +
            TeamMatchDBAdapter.COLUMN_NAME_START_LOCATION_ON_FIELD + TEXT_TYPE + COMMA_SEP +
    		TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_NOTES + TEXT_TYPE + COMMA_SEP +
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
            TeamMatchDBAdapter.COLUMN_NAME_AUTO_MODE_SAVED + BOOL_TYPE +
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
                TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TRANSACTION_READY_TO_EXPORT + TEXT_TYPE +
            ");",

            "SELECT * FROM "  + TeamMatchTransactionDataDBAdapter.TABLE_NAME,
            "DROP TABLE IF EXISTS " + TeamMatchTransactionDataDBAdapter.TABLE_NAME,
            "SELECT * FROM "  + TeamMatchTransactionDataDBAdapter.TABLE_NAME
        },

            {
                //9
                //TEAM_MATCH_TRANSACTIONS
                TeamMatchTransactionsDBAdapter.TABLE_NAME,
                "CREATE TABLE " + TeamMatchTransactionsDBAdapter.TABLE_NAME + " (" +
                    AUTO_INC_ID +
                    TeamMatchTransactionsDBAdapter.COLUMN_NAME_TEAM_MATCH_ID + INT_TYPE + COMMA_SEP +
                    TeamMatchTransactionsDBAdapter.COLUMN_NAME_TRANSACTION_ID + INT_TYPE +
               ");",

                "SELECT * FROM "  + TeamMatchTransactionsDBAdapter.TABLE_NAME,
                "DROP TABLE IF EXISTS " + TeamMatchTransactionsDBAdapter.TABLE_NAME,
                "SELECT * FROM "  + TeamMatchTransactionsDBAdapter.TABLE_NAME
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

        final String HELPER_KEY = "FTSHelperKey";

        // Register the database helper, so it can be shared with the SymmetricService
        SQLiteOpenHelperRegistry.register(HELPER_KEY, DBHelper);
        Intent intent = new Intent(ctx, SymmetricService.class);

        // Notify the service of the database helper key
        intent.putExtra(SymmetricService.INTENTKEY_SQLITEOPENHELPER_REGISTRY_KEY, HELPER_KEY);
        intent.putExtra(SymmetricService.INTENTKEY_REGISTRATION_URL, "http://10.0.0.100:32665/sync/fts-master");
        intent.putExtra(SymmetricService.INTENTKEY_EXTERNAL_ID, "001");
        intent.putExtra(SymmetricService.INTENTKEY_NODE_GROUP_ID, "fts-node");
        intent.putExtra(SymmetricService.INTENTKEY_START_IN_BACKGROUND, true);

        // initial load existing notes from the Client to the Server
        Properties properties = new Properties();
        properties.setProperty(ParameterConstants.AUTO_RELOAD_REVERSE_ENABLED, "true");
        intent.putExtra(SymmetricService.INTENTKEY_PROPERTIES, properties);

        ctx.startService(intent);
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
        }

        @Override
        public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) 
        {               
        	String query = "";
        	FTSUtilities.printToConsole("Upgrading tables in DBAdapter::DatabaseHelper -- Old DB Version: " + oldVersion + " - New DB Version: " + newVersion);

        	for(TABLE_NAMES table : TABLE_NAMES.values()) {
                FTSUtilities.printToConsole("Backing up data from table: " + table + "\n\n");
                query = TABLE_LIST[table.getIndex()][BACKUP_TABLE_SQL];
                try {
                    Method m = table.CLASS().getMethod("restoreTableData", ArrayList.class);
                }catch (Exception e) {
                    FTSUtilities.printToConsole("**** restoreTableData not found for: " + table.className);
                }
                Cursor c = db.rawQuery(query, null);

                ArrayList<HashMap<String, String>> data = new ArrayList<HashMap<String, String>>();
                while(c.moveToNext()) {
                    HashMap<String, String> datum = new HashMap<String, String>();
                    for(String column : c.getColumnNames()) {
                        datum.put(column, c.getString(c.getColumnIndex(column)));
                    }
                    data.add(datum);
                }

                FTSUtilities.printToConsole("Deleting/Re-Creating table: " + table + "\n\n");
        		query = TABLE_LIST[table.getIndex()][DELETE_TABLE_SQL];
        		FTSUtilities.printToConsole("Delete query: " + query + "\n\n");
        		db.execSQL(query);

                query = TABLE_LIST[table.getIndex()][CREATE_TABLE_SQL];
        		FTSUtilities.printToConsole("Create query: " + query + "\n\n");
        		db.execSQL(query);

                FTSUtilities.printToConsole("Restoring data to table: " + table + "\n\n");
                query = TABLE_LIST[table.getIndex()][RESTORE_TABLE_SQL];

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
                        db.insert(TABLE_LIST[table.getIndex()][TABLE_NAME], null, initialValues);
                    }
                }
                //c = db.rawQuery(query, null);
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
        this.DBHelper.close();
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
}
