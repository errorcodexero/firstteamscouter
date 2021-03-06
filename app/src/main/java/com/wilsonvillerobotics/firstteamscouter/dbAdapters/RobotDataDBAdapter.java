package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.provider.BaseColumns;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.ArrayList;
import java.util.HashMap;

public class RobotDataDBAdapter extends FTSDBAdapter implements BaseColumns, FTSTable {
	public static final String TABLE_NAME = "robot_data";

    // Columns
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
    //public static final String COLUMN_NAME_READY_TO_EXPORT = "ready_to_export";

    public static String[] allColumns = {
            _ID,
            COLUMN_NAME_TABLET_ID,
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
            COLUMN_NAME_ROBOT_STACKS_FROM,
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
    public RobotDataDBAdapter(Context ctx) {
        super(ctx);
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
    public RobotDataDBAdapter openForWrite() throws SQLException {
        return (RobotDataDBAdapter)openDBForWrite();
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
        return (RobotDataDBAdapter)openDBForRead();
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
            initialValues.put(COLUMN_NAME_TABLET_ID, FTSUtilities.wifiID);
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
            initialValues.put(COLUMN_NAME_READY_TO_EXPORT, Boolean.TRUE.toString());
        }
        long id = this.openForWrite().mDb.insert(TABLE_NAME, null, initialValues);
        if(!this.dbIsClosed()) this.close();
        return id;
    }

    /**
     * Delete the entry with the given rowId
     * 
     * @param rowId
     * @return true if deleted, false otherwise
     */
    public boolean deleteEntry(long rowId) {
        boolean retVal = this.openForWrite().mDb.delete(TABLE_NAME, _ID + "=" + rowId, null) > 0;
        if(!this.dbIsClosed()) this.close();
        return retVal;
    }

    @Override
    public boolean deleteAllEntries() {
        return super.deleteAllEntries(TABLE_NAME);
    }

    /**
     * Return a Cursor over the list of all entries in the database
     * 
     * @return Cursor over all Match Data entries
     */
    public Cursor getAllRobotDataEntries() {
        return this.openForRead().mDb.query(TABLE_NAME, allColumns, null, null, null, null, null);
    }

    @Override
    public Cursor getEntry(long rowID) {
        return super.getEntry(rowID, TABLE_NAME, allColumns);
    }

    /**
     * Return a Cursor over the list of all entries in the database
     *
     * @return Cursor over all entries
     */
    @Override
    public Cursor getAllEntries() {
        return super.getAllEntries(TABLE_NAME, allColumns);
    }

    public HashMap<String, String> getRobotDataEntry(long robotId) throws SQLException {
        HashMap<String, String> values = new HashMap<String, String>();
        Cursor mCursor = null;
        try {
            mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns,
                    _ID + "=" + robotId, null, null, null, null, null);
            if (mCursor.moveToFirst()) {
                for(String k : mCursor.getColumnNames()) {
                    values.put(k, mCursor.getString(mCursor.getColumnIndex(k)));
                }
            }
        } catch (Exception e) {

        } finally {
            if(mCursor != null && !mCursor.isClosed()) mCursor.close();
            if(!this.dbIsClosed()) this.close();
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
            mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns,
                    WHERE, null, null, null, null, null);
            if (mCursor.moveToFirst()) {
                id = mCursor.getLong(mCursor.getColumnIndex(_ID));
                mCursor.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if(mCursor != null && !mCursor.isClosed()) mCursor.close();
            if(!this.dbIsClosed()) this.close();
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
            mCursor = this.openForRead().mDb.query(true, TABLE_NAME, allColumns,
                    WHERE, null, null, null, null, null);
            if (mCursor.moveToFirst()) {
                for(String k : mCursor.getColumnNames()) {
                    values.put(k, mCursor.getString(mCursor.getColumnIndex(k)));
                }
            }
        } catch (Exception e) {

        } finally {
            if(mCursor != null && !mCursor.isClosed()) mCursor.close();
            if(!this.dbIsClosed()) this.close();
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
    public boolean updateRobotDataEntry(long rowId, long team_id, long competition_id, HashMap<String, String> values, Boolean export){
        ContentValues args = new ContentValues();
        args.put(COLUMN_NAME_TEAM_ID, team_id);
        args.put(COLUMN_NAME_COMPETITION_ID, competition_id);
        for(String k : values.keySet()) {
            args.put(k, values.get(k));
        }
        String exp = String.valueOf(export);
        args.put(COLUMN_NAME_READY_TO_EXPORT, exp);
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
}
