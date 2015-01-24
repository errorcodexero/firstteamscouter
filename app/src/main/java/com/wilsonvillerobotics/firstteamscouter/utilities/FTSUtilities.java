package com.wilsonvillerobotics.firstteamscouter.utilities;

import java.io.File;
import java.util.Hashtable;
import java.util.Set;

import com.wilsonvillerobotics.firstteamscouter.TeamMatchData.BALL_CONTROL;
import com.wilsonvillerobotics.firstteamscouter.TeamMatchData.ROBOT_ROLE;
import com.wilsonvillerobotics.firstteamscouter.TeamMatchData.ZONE;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;

import android.app.Activity;
import android.bluetooth.BluetoothAdapter;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.net.Uri;
import android.os.Environment;
import android.widget.Button;

public class FTSUtilities {

	public static Boolean DEBUG = true;
	public static Boolean POPULATE_TEST_DATA = true;
	public static String alliancePositions[] = {"Red1","Red2","Red3","Blue1","Blue2","Blue3",};
	
	public enum ALLIANCE_POSITION {
		RED1 (0, "Red1"),
		RED2 (1, "Red2"),
		RED3 (2, "Red3"),
		BLUE1 (3, "Blue1"),
		BLUE2 (4, "Blue2"),
		BLUE3 (5, "Blue3"),
		NOT_SET (6, "Not Set");
		
		private int id;
		private String strAlliancePositionString;
		ALLIANCE_POSITION(int id, String alliancePosition) {
			this.id = id;
			this.strAlliancePositionString = alliancePosition;
		}
		
		public int allianceID() {
			return this.id;
		}
		
		public String myAlliancePosition() {
			return this.strAlliancePositionString;
		}
		
		public static String getAlliancePositionForID(int id) {
			switch(id) {
			case 0: 
				return RED1.strAlliancePositionString;
			case 1:
				return RED2.strAlliancePositionString;
			case 2:
				return RED3.strAlliancePositionString;
			case 3:
				return BLUE1.strAlliancePositionString;
			case 4:
				return BLUE2.strAlliancePositionString;
			case 5:
				return BLUE3.strAlliancePositionString;
			default:
				return NOT_SET.strAlliancePositionString;
			}
		}
	}

	private static Hashtable<Integer, String> testTeamData = new Hashtable<Integer, String>(){
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{ 
			put(1425, "Error Code Xero");
			put(1520, "Flaming Chickens");
			put(2929, "JagBots");
			put(1114, "Derp");
			put(500, "Spots");
			put(600, "Frozen Poultry");
			put(700, "Jumping Frogs");
			put(800, "Droids");
			put(900, "Sad Pandas");
			put(1000, "Grommets");
		}
	};
	
	public static void printToConsole(String message) {
		if(message.isEmpty() || !DEBUG) return;
		int len = 50 - message.length();
		String spacer = "";
		for(int i = 0; i < len/2;i++) {
			spacer += " ";
		}
		System.out.println("");
		System.out.println("**************************************************");
		System.out.println(spacer + message);
		System.out.println("**************************************************");
		System.out.println("");
	}
	
	public static Set<Integer> getTestTeamNumbers() {
		Set<Integer> teamNums = testTeamData.keySet();
		return teamNums;
	}
	
	public static String getTeamName(int tNum) {
		return testTeamData.get(tNum);
	}
	
	public static boolean isExternalStorageWritable() {
	    String state = Environment.getExternalStorageState();
	    if (Environment.MEDIA_MOUNTED.equals(state)) {
	        return true;
	    }
	    return false;
	}
	
	/* Checks if external storage is available to at least read */
	public static boolean isExternalStorageReadable() {
	    String state = Environment.getExternalStorageState();
	    if (Environment.MEDIA_MOUNTED.equals(state) ||
	        Environment.MEDIA_MOUNTED_READ_ONLY.equals(state)) {
	        return true;
	    }
	    return false;
	}
	
	public static boolean initializeBluetooth(Activity activity) {
		BluetoothAdapter mBluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
		if (mBluetoothAdapter == null) {
		    // Device does not support Bluetooth
			return false;
		}
		
		int REQUEST_ENABLE_BT = 1;
		
		if (!mBluetoothAdapter.isEnabled()) {
		    Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
		    activity.startActivityForResult(enableBtIntent, REQUEST_ENABLE_BT);
		}
		
		
		return false;
	}
	
	public static Uri sendFileByBluetooth(Context context, String filePath, String btDeviceAddress) {
		//BluetoothAdapter btAdapter = BluetoothAdapter.getDefaultAdapter();
		
		//BluetoothDevice device = btAdapter.getRemoteDevice(btDeviceAddress);
		//String filePath = Environment.getExternalStorageDirectory().toString() + "/file.jpg";

		ContentValues values = new ContentValues();
		values.put(BluetoothShare.URI, Uri.fromFile(new File(filePath)).toString());
		values.put(BluetoothShare.DESTINATION, btDeviceAddress); // device.getAddress());
		values.put(BluetoothShare.DIRECTION, BluetoothShare.DIRECTION_OUTBOUND);
		Long ts = System.currentTimeMillis();
		values.put(BluetoothShare.TIMESTAMP, ts);
		Uri contentUri = context.getContentResolver().insert(BluetoothShare.CONTENT_URI, values);
		return contentUri;
	}
	
	public static void setButtonStyles(Hashtable<Integer, Button> buttonHash, Boolean undo) {
		int color = (undo) ? Color.RED : Color.BLUE;
		
		for(Integer bID : buttonHash.keySet()) {
			buttonHash.get(bID).setTextColor(color);
		}
	}
	
	private static String getIntCSVHeader() {
		String COMMA = ", ";
		String retVal = "";
		retVal += TeamMatchDBAdapter.COLUMN_NAME_START_LOCATION + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_SCORE + COMMA; 
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_HI_SCORE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_HI_HOT + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_HI_MISS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_LO_SCORE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_LO_HOT + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_LO_MISS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_COLLECT + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_COLLECT + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TELE_SCORE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TELE_HI_SCORE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TELE_HI_MISS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TELE_LO_SCORE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TELE_LO_MISS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ASSIST_RED + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ASSIST_WHITE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ASSIST_BLUE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_DEFEND_RED + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_DEFEND_WHITE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_DEFEND_BLUE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_DEFEND_GOAL + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TRUSS_TOSS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TRUSS_MISS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TOSS_CATCH + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TOSS_MISS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_SHORT_PASS_SUCCESS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_SHORT_PASS_MISS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_LONG_PASS_SUCCESS + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_LONG_PASS_MISS;
		
		return retVal;
	}

	private static String getBoolCSVHeader() {
		String COMMA = ", ";
		String retVal = "";
		retVal += TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_HAS_SAVED_DATA + COMMA;
		//retVal += TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_DATA_READY_TO_EXPORT + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_AUTO_MOVE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_BROKE_DOWN + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_NO_MOVE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_LOST_CONNECTION + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ROLE_SHOOTER + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ROLE_DEFENDER + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ROLE_PASSER + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ROLE_CATCHER + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_ROLE_GOALIE + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_BALL_CONTROL_GROUND_PICKUP + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_BALL_CONTROL_HUMAN_LOAD + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_BALL_CONTROL_HI_TO_LO + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_BALL_CONTROL_LO_TO_HI + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_BALL_CONTROL_HI_TO_HI + COMMA;
		retVal += TeamMatchDBAdapter.COLUMN_NAME_BALL_CONTROL_LO_TO_LO;
		
		return retVal;
	}
	
	public static String getCSVHeaderString() {
		String COMMA = ", ";
		String headerOut = "tablet_id" + COMMA;
        headerOut += TeamMatchDBAdapter._ID + COMMA + TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID + COMMA;
        headerOut += TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID + COMMA;
        headerOut += FTSUtilities.getIntCSVHeader() + COMMA;
        headerOut += FTSUtilities.getBoolCSVHeader() + COMMA;
        headerOut += TeamMatchDBAdapter.COLUMN_NAME_TEAM_MATCH_NOTES + "\n";
        return headerOut;
	}
}
