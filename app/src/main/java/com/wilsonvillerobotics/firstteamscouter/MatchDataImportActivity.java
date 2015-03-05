package com.wilsonvillerobotics.firstteamscouter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.channels.FileChannel;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.database.SQLException;
import android.os.Bundle;
import android.os.Environment;
import android.util.DisplayMetrics;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

public class MatchDataImportActivity extends Activity {

	protected static final int TIMER_RUNTIME = 10000; // in ms --> 10s
	
	private MatchDataDBAdapter matchDataDBAdapter;
	private TeamMatchDBAdapter teamMatchDBAdapter;
	private TeamDataDBAdapter teamDataDBAdapter;
    private RobotDataDBAdapter robotDataDBAdapter;

	private Button btnOK;
    private Button btnDisplayMetrics;
	private TextView txtStatus;
	private TextView txtTestDataAlert;
	protected ProgressBar mProgressBar;
	protected String tabletID;
	
	private String exportTeamMatchDataFileNamePrefix;
    private String exportTeamDataFileNamePrefix;
    private String exportRobotDataFileNamePrefix;

    private String tempFileNameTeamMatchData;
    private String tempFileNameTeamData;
    private String tempFileNameRobotData;

	private String csvExt;
	
	private int BLUETOOTH_SEND = 32665;
	
	private File filePath;
	private File myTeamMatchDataExportFile;
    private File myTeamDataExportFile;
    private File myRobotDataExportFile;
	private File myTeamMatchDataTempFile;
    private File myTeamDataTempFile;
    private File myRobotDataTempFile;
	private File saveDir;
	private File exportDir;

	private int numTestMatches;
	
	//getExternalStorageState()

	@SuppressLint("WorldReadableFiles")
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_data_import);
		
		this.numTestMatches = 25;
		
		Intent intent = getIntent();
		this.tabletID = intent.getStringExtra("tablet_id");
		this.tabletID = (this.tabletID != null) ? this.tabletID : "Unknown Tablet ID";
		
		this.csvExt = ".csv";
		this.exportTeamMatchDataFileNamePrefix = tabletID + "_team_match_data_export";
        this.exportTeamDataFileNamePrefix = tabletID + "_team_data_export";
        exportRobotDataFileNamePrefix = tabletID + "_robot_data_export";
		this.tempFileNameTeamMatchData = exportTeamMatchDataFileNamePrefix + this.csvExt;
        this.tempFileNameTeamData = exportTeamDataFileNamePrefix + this.csvExt;
        this.tempFileNameRobotData = exportRobotDataFileNamePrefix + this.csvExt;
		
		
		this.filePath = getExternalFilesDir(null);
		this.myTeamMatchDataExportFile = null;
        this.myTeamDataExportFile = null;
		this.myTeamMatchDataTempFile = new File(filePath, tempFileNameTeamMatchData);
        this.myTeamDataExportFile = new File(filePath, tempFileNameTeamMatchData);
        this.myRobotDataExportFile = new File(filePath, tempFileNameTeamMatchData);
		this.saveDir = new File(filePath.getAbsolutePath() + "/sent");
		this.exportDir = new File(filePath.getAbsolutePath() + "/exported");
		
		try {
			FTSUtilities.printToConsole("ImportMatchDataActivity::onCreate : OPENING DB\n");
			matchDataDBAdapter = new MatchDataDBAdapter(this).open();
			teamMatchDBAdapter = new TeamMatchDBAdapter(this).open();
			teamDataDBAdapter = new TeamDataDBAdapter(this).open();
            robotDataDBAdapter = new RobotDataDBAdapter(this).open();
		} catch(SQLException e) {
			e.printStackTrace();
			matchDataDBAdapter = null;
			teamMatchDBAdapter = null;
			teamDataDBAdapter = null;
            robotDataDBAdapter = null;
		}
		
		String statusMessage;
		String testDataAlertMessage = "";
		if(FTSUtilities.POPULATE_TEST_DATA) {
			statusMessage = "Press the import button to import test data for " + numTestMatches + " teams\n";
			testDataAlertMessage = "TEST DATA MODE";
		} else {
			statusMessage = "Press the 'Import' button to import matches from 'match_list_data.csv'\nExpected format is:\nTime : Match Type : Match Number : Red1 : Red2 : Red3 : Blue1 : Blue2 : Blue3";
		}
		
		txtStatus = (TextView) findViewById(R.id.txtStatus);
	    txtStatus.setText(statusMessage);
	    
	    txtTestDataAlert = (TextView) findViewById(R.id.txtTestDataAlert);
	    txtTestDataAlert.setText(testDataAlertMessage);
	    
	    mProgressBar = (ProgressBar)findViewById(R.id.progressBar1);

        btnDisplayMetrics = (Button) findViewById(R.id.btnDisplayMetrics);
        btnDisplayMetrics.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                displayScreenMetrics();
            }
        });

		btnOK = (Button) findViewById(R.id.btnImportMatchData);
		btnOK.setOnClickListener(new View.OnClickListener() {
			
			@Override
			public void onClick(View v) {
				String importStatusMessage = "";
				try {
					if(FTSUtilities.POPULATE_TEST_DATA) {
						teamMatchDBAdapter.populateTestData(matchDataDBAdapter.populateTestData(numTestMatches), teamDataDBAdapter.populateTestData());
						importStatusMessage = "Test data import complete";
					} else {
					    String storageState = Environment.getExternalStorageState();
					    if (storageState.equals(Environment.MEDIA_MOUNTED)) {
					    	FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : getting file\n");
					        File file = new File(getExternalFilesDir(null), "match_list_data.csv");
					        FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : file " + ((file.exists()) ? "EXISTS" : "IS MISSING") + "\n");

					        if(file.exists() && file.isFile()) {
					        	txtStatus.setText("File Found, Import Commencing\n");
                                teamDataDBAdapter.deleteAllData();
                                matchDataDBAdapter.deleteAllData();
                                teamMatchDBAdapter.deleteAllData();
                                robotDataDBAdapter.getAllRobotDataEntries();

						        BufferedReader inputReader = new BufferedReader(
						                new InputStreamReader(new FileInputStream(file)));
						        String line;
						        int lineCount = 0;
						        int matchCount = 0;
						        int teamCount = 0;
						        String lineArray[];
						        inputReader.mark((int)file.length());
						        line = inputReader.readLine();
						        lineArray = line.split(",");
						        //String headerArray[] = {"Time", "Type", "#", FTSUtilities.alliancePositions[0], FTSUtilities.alliancePositions[1], FTSUtilities.alliancePositions[2],
						        //		FTSUtilities.alliancePositions[3], FTSUtilities.alliancePositions[4], FTSUtilities.alliancePositions[5]};

						        if(lineArray[1].startsWith("Type")) {
						        	FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : Header Row Detected");
						        } else {
						        	FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : NO Heasder Row Detected");
						        	inputReader.reset();
						        }

						        while((line = inputReader.readLine()) != null) {
						        	lineCount += 1;
						        	lineArray = line.split(",");

						        	if(lineArray.length > 8) {
						        		//FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : " + lineArray[0] + ":" + lineArray[1] + ":" + lineArray[2] + ":" + lineArray[3] + ":" + lineArray[4] + ":" + lineArray[5] + ":" + lineArray[6] + ":" + lineArray[7]);
						        		//Time : Type : MatchNum : Red1 : Red2 : Red3 : Blue1 : Blue2 : Blue3
						        		long teamIDs[] = {-1, -1, -1, -1, -1, -1};
						        		for(int i = 0; i < 6; i++) {
						        			teamIDs[i] = teamDataDBAdapter.createTeamDataEntry(Integer.parseInt(lineArray[i+3]), 0)[0];
						        		}

						        		long matchID = matchDataDBAdapter.createMatchData(lineArray[0], lineArray[1], lineArray[2], teamIDs[0], teamIDs[1], teamIDs[2], teamIDs[3], teamIDs[4], teamIDs[5]);
						        		if(matchID >= 0) {
						        			matchCount += 1;
						        		}

						        		long teamMatchID;
						        		for(int i = 0; i < FTSUtilities.ALLIANCE_POSITION.NOT_SET.allianceIndex(); i++) {
						        			teamMatchID = teamMatchDBAdapter.createTeamMatch(FTSUtilities.ALLIANCE_POSITION.getAlliancePositionForIndex(i) /*.alliancePositions[i]*/, teamIDs[i], matchID);
						        			if(teamMatchID >= 0) {
						        				teamCount += 1;
						        			}
						        		}
						        	} else {
						        		FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : line not in proper format: " + line);
						        	}
						        }
						        inputReader.close();

						        importStatusMessage = txtStatus.getText() + "\nLines Parsed: " + lineCount + "\nMatches Created: " + matchCount + "\nTeamMatch Records Created: " + teamCount;
					        } else {
					        	importStatusMessage = "ERROR: could not find file:\n" + file.toString();
					        }



					        file.renameTo(new File(file.getAbsolutePath() + ".bak"));
					    }
					}
				} catch (Exception e) {
					FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : ERROR");
					importStatusMessage = "ERROR importing data";
				    e.printStackTrace();
				}
				txtStatus.setText(importStatusMessage);
			}
		});
	}
	
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		FTSUtilities.printToConsole("ImportMatchDataActivity::onActivityResult : requestCode: " + requestCode);
	    // Check which request we're responding to
	    if (requestCode == BLUETOOTH_SEND) {
	    	FTSUtilities.printToConsole("ImportMatchDataActivity::onActivityResult : Sharing Activity Returned");
	    	Toast.makeText(getBaseContext(), "File sent", Toast.LENGTH_SHORT).show();
//			if(saveDir.mkdir() || saveDir.isDirectory()) {
//				int numFiles = (saveDir.list(new CSVFilenameFilter(".csv")).length) + 1;
//				String saveFileName = exportFileName + "_" + numFiles + ".csv";
//				File saveFile = new File(saveDir, saveFileName);
//				if(myTeamMatchDataExportFile.exists()) {
//					myTeamMatchDataExportFile.renameTo(saveFile);
//					Toast.makeText(this, "File Saved", 3).show();
//				}
//			}
	    }
	}
	
	public void copy(File src, File dst) throws IOException {
	    FileInputStream inStream = new FileInputStream(src);
	    FileOutputStream outStream = new FileOutputStream(dst);
	    FileChannel inChannel = inStream.getChannel();
	    FileChannel outChannel = outStream.getChannel();
	    inChannel.transferTo(0, inChannel.size(), outChannel);
	    inStream.close();
	    outStream.close();
	}

    /*
	public void updateProgress(final int timePassed) {
       if(null != mProgressBar) {
           // Ignore rounding error here
           final int progress = mProgressBar.getMax() * timePassed / TIMER_RUNTIME;
           mProgressBar.setProgress(progress);
       }
   }
   */

	public void onContinue() {
	     // perform any final actions here
   }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if(matchDataDBAdapter == null) {
        	matchDataDBAdapter = new MatchDataDBAdapter(this.getBaseContext());
        }
        matchDataDBAdapter.open();
        
        if(teamMatchDBAdapter == null) {
        	teamMatchDBAdapter = new TeamMatchDBAdapter(this.getBaseContext());
        }
        teamMatchDBAdapter.open();

        if(teamDataDBAdapter == null) {
        	teamDataDBAdapter = new TeamDataDBAdapter(this.getBaseContext());
        }
        teamDataDBAdapter.open();
    }

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onResume() {
        super.onResume();
        
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
    }

    @Override
    protected void onStop() {
        super.onStop();
        if(myTeamMatchDataTempFile.exists()) myTeamMatchDataTempFile.delete();
        FTSUtilities.printToConsole("ImportMatchDataActivity::onStop : CLOSING DB\n");
        matchDataDBAdapter.close();
        teamMatchDBAdapter.close();
        teamDataDBAdapter.close();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.enter_data, menu);
		return true;
	}

    private void displayScreenMetrics() {
        DisplayMetrics metrics = new DisplayMetrics();
        getWindowManager().getDefaultDisplay().getMetrics(metrics);
        String msg = "";
        switch(metrics.densityDpi){
            case DisplayMetrics.DENSITY_LOW:
                msg = "Low density screen\n";
                break;
            case DisplayMetrics.DENSITY_MEDIUM:
                msg = "Medium density screen\n";
                break;
            case DisplayMetrics.DENSITY_HIGH:
                msg = "High density screen\n";
                break;
            case DisplayMetrics.DENSITY_XHIGH:
                msg = "Extra High density screen\n";
                break;
            case DisplayMetrics.DENSITY_TV:
                msg = "TV density screen\n";
                break;
        }
        msg += "Width: " + metrics.widthPixels + " pix    Height: " + metrics.heightPixels + " pix\n";
        msg += "X-DPI: " + metrics.xdpi + "Y-DPI: " + metrics.ydpi + "\n";
        msg += "Density: " + metrics.density;
        Toast.makeText(getBaseContext(), msg, Toast.LENGTH_LONG).show();
    }
}