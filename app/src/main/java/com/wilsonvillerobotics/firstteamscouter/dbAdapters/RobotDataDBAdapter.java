package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

import java.util.ArrayList;
import java.util.HashMap;

public class RobotDataDBAdapter implements BaseColumns {
	public static final String TABLE_NAME = "robot_data";
    public static final String COLUMN_NAME_TEAM_ID = "team_id";
    public static final String COLUMN_NAME_COMPETITION_ID = "competition_id";
    public static final String COLUMN_NAME_DRIVE_TRAIN_TYPE = "drive_train_type";
    public static final String COLUMN_NAME_WHEEL_TYPE = "wheel_type";
    public static final String COLUMN_NAME_NUMBER_WHEELS = "number_of_wheels";
    public static final String COLUMN_NAME_NUMBER_TOTE_STACKS = "number_of_tote_stacks";
    public static final String COLUMN_NAME_NUMBER_TOTES_PER_STACK = "number_of_totes_per_stack";
    public static final String COLUMN_NAME_NUMBER_CANS_AT_ONCE = "number_of_cans_robot_can_handle";
    public static final String COLUMN_NAME_GET_STEP_CANS = "robot_can_get_step_cans";
    public static final String COLUMN_NAME_PUT_TOTES_ON_STEP = "robot_can_put_totes_on_step";
    public static final String COLUMN_NAME_ROBOT_SOFTWARE_LANGUAGE = "robot_software_language";
    public static final String COLUMN_NAME_TOTE_MANIPULATOR_TYPE = "tote_manipulator_type";
    public static final String COLUMN_NAME_CAN_MANIPULATOR_TYPE = "can_manipulator_type";
    public static final String COLUMN_NAME_ROBOT_DRIVE_RANGE = "robot_drive_range";
    public static final String COLUMN_NAME_COOPERTITION = "team_does_coopertition";
    public static final String COLUMN_NAME_ROBOT_STACKS_FROM = "robot_stacks_from";


    private DatabaseHelper mDbHelper;
    private SQLiteDatabase mDb;

    private final Context mCtx;

    private final String allColumns[] = {
            _ID,
            COLUMN_NAME_TEAM_ID,
            COLUMN_NAME_COMPETITION_ID,
            COLUMN_NAME_DRIVE_TRAIN_TYPE,
            COLUMN_NAME_WHEEL_TYPE,
            COLUMN_NAME_NUMBER_WHEELS,
            COLUMN_NAME_NUMBER_TOTE_STACKS,
            COLUMN_NAME_NUMBER_TOTES_PER_STACK,
            COLUMN_NAME_NUMBER_CANS_AT_ONCE,
            COLUMN_NAME_GET_STEP_CANS,
            COLUMN_NAME_PUT_TOTES_ON_STEP,
            COLUMN_NAME_ROBOT_SOFTWARE_LANGUAGE,
            COLUMN_NAME_TOTE_MANIPULATOR_TYPE,
            COLUMN_NAME_CAN_MANIPULATOR_TYPE,
            COLUMN_NAME_ROBOT_DRIVE_RANGE,
            COLUMN_NAME_COOPERTITION,
            COLUMN_NAME_ROBOT_STACKS_FROM
    };

    private static class DatabaseHelper extends SQLiteOpenHelper {

        private static DatabaseHelper mInstance = null;

        private DatabaseHelper(Context context) {
            super(context, DBAdapter.DATABASE_NAME, null, DBAdapter.DATABASE_VERSION);
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
    public RobotDataDBAdapter(Context ctx) {
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
    public RobotDataDBAdapter openForWrite() throws SQLException {
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getWritableDatabase();
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
    public RobotDataDBAdapter openForRead() throws SQLException {
        this.mDbHelper = DatabaseHelper.getInstance(this.mCtx);
        this.mDb = this.mDbHelper.getReadableDatabase();
        return this;
    }

    /**
     * close return type: void
     */
    public void close() {
        this.mDbHelper.close();
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
     * @param values
     * @return rowId or -1 if failed
     */
    public long createRobotDataEntry(long team_id, long competition_id, HashMap<String, String> values){
        ContentValues initialValues = new ContentValues();
        if(values != null) {
            initialValues.put(COLUMN_NAME_TEAM_ID, team_id);
            initialValues.put(COLUMN_NAME_COMPETITION_ID, competition_id);
            initialValues.put(COLUMN_NAME_DRIVE_TRAIN_TYPE, values.get(COLUMN_NAME_DRIVE_TRAIN_TYPE));
            initialValues.put(COLUMN_NAME_WHEEL_TYPE, values.get(COLUMN_NAME_WHEEL_TYPE));
            initialValues.put(COLUMN_NAME_NUMBER_WHEELS, values.get(COLUMN_NAME_NUMBER_WHEELS));
            initialValues.put(COLUMN_NAME_NUMBER_TOTE_STACKS, values.get(COLUMN_NAME_NUMBER_TOTE_STACKS));
            initialValues.put(COLUMN_NAME_NUMBER_TOTES_PER_STACK, values.get(COLUMN_NAME_NUMBER_TOTES_PER_STACK));
            initialValues.put(COLUMN_NAME_NUMBER_CANS_AT_ONCE, values.get(COLUMN_NAME_NUMBER_CANS_AT_ONCE));
            initialValues.put(COLUMN_NAME_GET_STEP_CANS, values.get(COLUMN_NAME_GET_STEP_CANS));
            initialValues.put(COLUMN_NAME_PUT_TOTES_ON_STEP, values.get(COLUMN_NAME_PUT_TOTES_ON_STEP));
            initialValues.put(COLUMN_NAME_ROBOT_SOFTWARE_LANGUAGE, values.get(COLUMN_NAME_ROBOT_SOFTWARE_LANGUAGE));
            initialValues.put(COLUMN_NAME_TOTE_MANIPULATOR_TYPE, values.get(COLUMN_NAME_TOTE_MANIPULATOR_TYPE));
            initialValues.put(COLUMN_NAME_CAN_MANIPULATOR_TYPE, values.get(COLUMN_NAME_CAN_MANIPULATOR_TYPE));
            initialValues.put(COLUMN_NAME_ROBOT_DRIVE_RANGE, values.get(COLUMN_NAME_ROBOT_DRIVE_RANGE));
            initialValues.put(COLUMN_NAME_COOPERTITION, values.get(COLUMN_NAME_COOPERTITION));
            initialValues.put(COLUMN_NAME_ROBOT_STACKS_FROM, values.get(COLUMN_NAME_ROBOT_STACKS_FROM));
        }
        return this.mDb.insert(TABLE_NAME, null, initialValues);
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteRobotDataEntry(long rowId) {

        return this.mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllRobotDataEntries() {
        return this.mDb.query(TABLE_NAME, allColumns, null, null, null, null, null);
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param robotId
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public HashMap<String, String> getRobotDataEntry(long robotId) throws SQLException {
        HashMap<String, String> values = new HashMap<String, String>();
        Cursor mCursor = null;
        try {
            mCursor = this.mDb.query(true, TABLE_NAME, allColumns,
                    _ID + "=" + robotId, null, null, null, null, null);
            if (mCursor.moveToFirst()) {
                for(String k : mCursor.getColumnNames()) {
                    values.put(k, mCursor.getString(mCursor.getColumnIndex(k)));
                }
            }
        } catch (Exception e) {

        } finally {
            if(mCursor != null && !mCursor.isClosed()) mCursor.close();
        }

        return values;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param team_id
     * @param competition_id
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public long getRobotIdForTeamAtCompetition(long team_id, long competition_id) throws SQLException {
        long id = -1;
        String WHERE = COLUMN_NAME_TEAM_ID + "=" + String.valueOf(team_id);
        WHERE += " AND " + COLUMN_NAME_COMPETITION_ID + "=" + String.valueOf(competition_id);

        Cursor mCursor = null;
        try {
            mCursor = this.mDb.query(true, TABLE_NAME, allColumns,
                    WHERE, null, null, null, null, null);
            if (mCursor.moveToFirst()) {
                id = mCursor.getLong(mCursor.getColumnIndex(_ID));
                mCursor.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(mCursor != null && !mCursor.isClosed()) mCursor.close();
        }
        return id;
    }

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param team_id
     * @param competition_id
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    public HashMap<String, String> getRobotDataEntryForTeamAtCompetition(long team_id, long competition_id) throws SQLException {
        HashMap<String, String> values = new HashMap<String, String>();
        Cursor mCursor = null;
        String WHERE = COLUMN_NAME_TEAM_ID + "=" + team_id;
        WHERE += " AND " + COLUMN_NAME_COMPETITION_ID + "=" + competition_id;

        try {
            mCursor = this.mDb.query(true, TABLE_NAME, allColumns,
                    WHERE, null, null, null, null, null);
            if (mCursor.moveToFirst()) {
                for(String k : mCursor.getColumnNames()) {
                    values.put(k, mCursor.getString(mCursor.getColumnIndex(k)));
                }
            }
        } catch (Exception e) {

        } finally {
            if(mCursor != null && !mCursor.isClosed()) mCursor.close();
        }

        return values;
    }

    /**
     * Update the entry.
     * 
     * @param rowId
     * @param values
     * @return true if the entry was successfully updated, false otherwise
     */
    public boolean updateRobotDataEntry(long rowId, long team_id, long competition_id, HashMap<String, String> values){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_COMPETITION_ID, competition_id);
        for(String k : values.keySet()) {
            args.put(k, values.get(k));
        }
        return this.mDb.update(TABLE_NAME, args, _ID + "=" + rowId, null) >0; 
    }

}
