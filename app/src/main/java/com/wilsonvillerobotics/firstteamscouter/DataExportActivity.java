package com.wilsonvillerobotics.firstteamscouter;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.ComponentName;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.net.Uri;
import android.os.Bundle;
import android.os.Environment;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.DBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionsDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.DataXmlExporter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTPFileUploader;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.Hashtable;

public class DataExportActivity extends Activity {

	protected static final int TIMER_RUNTIME = 10000; // in ms --> 10s

	private MatchDataDBAdapter matchDataDBAdapter;
	private TeamMatchDBAdapter teamMatchDBAdapter;
    private TeamMatchTransactionsDBAdapter tmtDBAdapter;
    private TeamMatchTransactionDataDBAdapter tmtdDBAdapter;
	private TeamDataDBAdapter teamDataDBAdapter;
    private RobotDataDBAdapter robotDataDBAdapter;
    private DBAdapter dbAdapter;

	protected ProgressBar mProgressBar;
	protected String tabletID;
    protected ArrayList<Long> lastMatchDataExportList;
    protected ArrayList<Long> lastMatchTransactionDataExportList;

	private String exportTeamMatchDataFileNamePrefix;

	private final String csvExt = ".csv";

	private int BLUETOOTH_SEND = 32665;

	private File filePath;
	private File myTeamMatchDataExportFile;
	private File myTeamMatchDataTempFile;
    private File saveDir;
	private File exportDir;

	//getExternalStorageState()

	@SuppressLint("WorldReadableFiles")
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_data_export);

        String statusMessage = getResources().getString(R.string.text_export_status) + "  Press the 'Export Via Bluetooth' button to export match data";

        initLastExportLists();
        processIntent();
        initFiles();
        openDatabase();
        updateStatus(statusMessage);
        configButtons();
	}

    private void initLastExportLists() {
        lastMatchDataExportList = new ArrayList<Long>();
        lastMatchTransactionDataExportList = new ArrayList<Long>();
    }

    private void processIntent() {
        Intent intent = getIntent();
        this.tabletID = intent.getStringExtra("tablet_id");
        this.tabletID = (this.tabletID != null) ? this.tabletID : "Unknown Tablet ID";
    }

    private void initFiles() {
        this.exportTeamMatchDataFileNamePrefix = tabletID + "_team_match_data_export";
        String tempFileNameTeamMatchData = exportTeamMatchDataFileNamePrefix + this.csvExt;

        this.filePath = getExternalFilesDir(null);
        this.myTeamMatchDataExportFile = null;
        this.myTeamMatchDataTempFile = new File(filePath, tempFileNameTeamMatchData);
        this.saveDir = new File(filePath.getAbsolutePath() + "/sent");
        this.exportDir = new File(filePath.getAbsolutePath() + "/exported");
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("ImportMatchDataActivity::onCreate : OPENING DB\n");
            dbAdapter = new DBAdapter(this);
            matchDataDBAdapter = new MatchDataDBAdapter(this);
            teamMatchDBAdapter = new TeamMatchDBAdapter(this);
            teamDataDBAdapter = new TeamDataDBAdapter(this);
            tmtDBAdapter = new TeamMatchTransactionsDBAdapter(this);
            tmtdDBAdapter = new TeamMatchTransactionDataDBAdapter(this);
            robotDataDBAdapter = new RobotDataDBAdapter(this);
        } catch(SQLException e) {
            e.printStackTrace();
            dbAdapter = null;
            matchDataDBAdapter = null;
            teamMatchDBAdapter = null;
            teamDataDBAdapter = null;
            tmtDBAdapter = null;
            tmtdDBAdapter = null;
            robotDataDBAdapter = null;
        }
    }

    private void updateStatus(String statusMessage) {
        TextView txtDataExportStatus = (TextView) findViewById(R.id.txtDataExportStatus);
        txtDataExportStatus.setText(statusMessage);
    }

    private void exportAllDataToXml() {
        int fileCount = dbAdapter.exportDatabase();
        updateStatus("Files exported: " + String.valueOf(fileCount));

        FTPFileUploader ftpUL = new FTPFileUploader();
        File dir = new File(Environment.getExternalStorageDirectory(), DataXmlExporter.DATASUBDIRECTORY);
        ftpUL.execute(dir.listFiles());
    }

    private void exportMatchData() {
        // TODO - export the db match data for this competition to CVS files if it hasn't already been exported
        // track list of last data exported to enable easy repeat of send if BT fails

        // Get data for matches that have not been exported
        Cursor c = teamMatchDBAdapter.getTeamMatchesWithDataToExport();
        if(c != null && c.getCount() > 0) {
            lastMatchDataExportList.clear();
            while (c.moveToNext()) {
                lastMatchDataExportList.add(c.getLong(c.getColumnIndex(TeamMatchDBAdapter._ID)));
                // TODO - set up the data to export and write it to a file - use an array of field IDs?
                //setItemsExported(ArrayList<Long> exportedItems)  -- finish implementing this
                /*
                _ID,
                COLUMN_NAME_TEAM_ID,
                COLUMN_NAME_MATCH_ID,
                COLUMN_NAME_TEAM_MATCH_ALLIANCE_POSITION,
                COLUMN_NAME_BROKE_DOWN,
                COLUMN_NAME_NO_MOVE,
                COLUMN_NAME_LOST_CONNECTION,
                COLUMN_NAME_START_LOCATION,
                COLUMN_NAME_AUTO_ROBOT_START_LOCATION_X,
                COLUMN_NAME_AUTO_ROBOT_START_LOCATION_Y,
                COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_X,
                COLUMN_NAME_AUTO_ROBOT_FINAL_LOCATION_Y,
                COLUMN_NAME_AUTO_TOTES_PICKED_UP,
                COLUMN_NAME_AUTO_TOTES_STACKED,
                COLUMN_NAME_AUTO_TOTES_SCORED,
                COLUMN_NAME_AUTO_CANS_PICKED_UP,
                COLUMN_NAME_AUTO_CANS_SCORED,
                COLUMN_NAME_AUTO_CANS_GRABBED_FROM_STEP,
                COLUMN_NAME_START_LOCATION_ON_FIELD,
                COLUMN_NAME_TEAM_MATCH_NOTES
                 */
            }
        }
    }

    private void exportMatchTransactionData() {
        // TODO - export the db match transaction data for this competition to CVS files if it hasn't already been exported
        // track list of last data exported to enable easy repeat of send if BT fails
    }

    private void configButtons() {
        Button btnRepeatDataExport = (Button) findViewById(R.id.btnRepeatDataExport);
        btnRepeatDataExport.setOnClickListener(new View.OnClickListener() {

			@Override
			public void onClick(View v) {
				if(exportDir.mkdir() || exportDir.isDirectory()) {
					File[] exportedFileList = exportDir.listFiles(new CSVFilenameFilter(".csv"));
					int fileCount = exportedFileList.length;

					File myReExportFile = new File(exportDir, exportTeamMatchDataFileNamePrefix + "_" + fileCount + csvExt);
					if(myReExportFile.exists() && myReExportFile.isFile()) {
						FTSUtilities.printToConsole("ExportMatchDataActivity::btnRepeatDataExport.onClick : Exporting: " + myReExportFile.getName());
						Intent sharingIntent = new Intent(Intent.ACTION_SEND);
						sharingIntent.setType("text/plain");
						sharingIntent.setComponent(new ComponentName("com.android.bluetooth", "com.android.bluetooth.opp.BluetoothOppLauncherActivity"));
						sharingIntent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(myReExportFile));
						startActivityForResult(sharingIntent, BLUETOOTH_SEND);
						FTSUtilities.printToConsole("ExportMatchDataActivity::btnRepeatDataExport.onClick : Sharing Activity Started");
					}
				}
			}
		});

        Button btnExportDataXML = (Button)findViewById(R.id.btnExportDataXML);
        btnExportDataXML.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                exportAllDataToXml();
            }
        });

        Button btnExportDataBluetooth = (Button) findViewById(R.id.btnExportDataBluetooth);
        btnExportDataBluetooth.setOnClickListener(new View.OnClickListener() {

			@Override
			public void onClick(View v) {
                // This relies on the data being written to CSV files as well as to the DB
                // TODO - update this to run a query to grab the data, make the CSV files, and then export it.
				String csvFiles[] = filePath.list();
				String exportedFiles[] = new String[1024];
				Hashtable<Integer, String> lines = new Hashtable<Integer, String>();
				int entryNum = 0;
				int exportNum = 0;

				for(String csvFile : csvFiles) {
					String localFileName = filePath.getAbsolutePath() + "/" + csvFile;
					File tempFile = new File(localFileName);
					if(tempFile.isFile()) {
						FTSUtilities.printToConsole("ExportMatchDataActivity::btnExportDataBluetooth : csvFile is a file: " + csvFile);
						if(csvFile.endsWith("match_data_export")) {
							exportedFiles[exportNum++] = csvFile;
							FTSUtilities.printToConsole("ExportMatchDataActivity::btnExportDataBluetooth : csvFile contains match_data_export: " + csvFile);
							FileReader fi = null;
							try {
								fi = new FileReader(tempFile);
							} catch (FileNotFoundException e) {
								e.printStackTrace();
							}
							try {
                                BufferedReader bufferedReader = new BufferedReader(fi);
                                String line = "";
                                int lineNum = 0;
                                while((line = bufferedReader.readLine()) != null) {
									if(lineNum > 0) {
										FTSUtilities.printToConsole("ExportMatchDataActivity::btnExportDataBluetooth : line: " + line);
										lines.put(entryNum++, line);
									}
									lineNum++;

									if(entryNum > 1023) break;
								}
							} catch (IOException e) {
								e.printStackTrace();
							} catch (NullPointerException e) {
                                e.printStackTrace();
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
						}
					} else {
						FTSUtilities.printToConsole("ExportMatchDataActivity::btnExportDataBluetooth : csvFile is NOT a file: " + csvFile);
					}
				}

				if(!lines.isEmpty() && (exportDir.mkdir() || exportDir.isDirectory())) {
					if(myTeamMatchDataExportFile != null && myTeamMatchDataExportFile.exists()) myTeamMatchDataExportFile.delete();

					FileOutputStream fo =  null;
					boolean append = true;
					try {
						File[] exportedFileList = exportDir.listFiles(new CSVFilenameFilter(".csv"));
						int fileCount = exportedFileList.length + 1;

						myTeamMatchDataExportFile = new File(filePath.getAbsolutePath() + "/" + exportTeamMatchDataFileNamePrefix + "_" + fileCount + csvExt);
						boolean fileCreated = myTeamMatchDataExportFile.createNewFile();
                        if(fileCreated) {
                            String header = FTSUtilities.getCSVHeaderString();
                            fo = new FileOutputStream(myTeamMatchDataExportFile, append);

                            fo.write(header.getBytes());
                            fo.close();
                        }
					} catch (IOException e) {
						e.printStackTrace();
					}

					if(myTeamMatchDataExportFile.exists()) {
						try {
							fo = new FileOutputStream(myTeamMatchDataExportFile, append);
							for(String line : lines.values()) {
								if(line != null) {
									line += "\n";
									fo.write(line.getBytes());
								}
							}
							fo.close();

							String exportedFileName = myTeamMatchDataExportFile.getName();
							File exportFile = new File(exportDir, exportedFileName);
							myTeamMatchDataExportFile.renameTo(exportFile);

							Intent sharingIntent = new Intent(Intent.ACTION_SEND);
							sharingIntent.setType("text/plain");
							sharingIntent.setComponent(new ComponentName("com.android.bluetooth", "com.android.bluetooth.opp.BluetoothOppLauncherActivity"));
							sharingIntent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(exportFile));
							startActivityForResult(sharingIntent, BLUETOOTH_SEND);
							FTSUtilities.printToConsole("ExportMatchDataActivity::btnExportDataBluetooth : Sharing Activity Started");
						} catch (FileNotFoundException e) {
							e.printStackTrace();
						} catch (IOException e) {
							e.printStackTrace();
						}

						if(saveDir.mkdir() || saveDir.isDirectory()) {
							entryNum = 0;
							for(String exportedFile : exportedFiles) {
								if(exportedFile != null) {
									FTSUtilities.printToConsole("ExportMatchDataActivity::btnExportDataBluetooth : exportedFile: " + exportedFile);
									File tempFile = new File(filePath.getAbsolutePath() + "/" + exportedFile);
									String saveFileName = exportedFile + csvExt;
									File saveFile = new File(saveDir, saveFileName);
									if(tempFile.renameTo(saveFile)) entryNum++;
								}
							}

							Toast.makeText(getBaseContext(), "File(s) Saved: " + entryNum, Toast.LENGTH_SHORT).show();
						}
					} else {
						String message = "File not found: " + myTeamMatchDataExportFile.getName();
						Toast.makeText(getBaseContext(), message , Toast.LENGTH_SHORT).show();
					}
				}
			}
		});
    }

    @Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		FTSUtilities.printToConsole("ExportMatchDataActivity::onActivityResult : requestCode: " + requestCode);
	    // Check which request we're responding to
	    if (requestCode == BLUETOOTH_SEND) {
	    	FTSUtilities.printToConsole("ExportMatchDataActivity::onActivityResult : Sharing Activity Returned");
	    	Toast.makeText(getBaseContext(), "File sent", Toast.LENGTH_SHORT).show();
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

	public void updateProgress(final int timePassed) {
       if(null != mProgressBar) {
           // Ignore rounding error here
           final int progress = mProgressBar.getMax() * timePassed / TIMER_RUNTIME;
           mProgressBar.setProgress(progress);
       }
   }

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
        matchDataDBAdapter.openForWrite();
        
        if(teamMatchDBAdapter == null) {
        	teamMatchDBAdapter = new TeamMatchDBAdapter(this.getBaseContext());
        }
        teamMatchDBAdapter.openForWrite();

        if(teamDataDBAdapter == null) {
        	teamDataDBAdapter = new TeamDataDBAdapter(this.getBaseContext());
        }
        teamDataDBAdapter.openForWrite();
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
        FTSUtilities.printToConsole("ExportMatchDataActivity::onStop : CLOSING DB\n");
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

    public class CSVFilenameFilter implements FilenameFilter {
		String ext;
		
		public CSVFilenameFilter(String ext) {
		    this.ext = ext; 
		}
		
		@SuppressLint("DefaultLocale")
		@Override
		public boolean accept(File dir, String filename) {
		   //If you want to perform a case-insensitive search
		   return filename.toLowerCase().endsWith(ext.toLowerCase());
		}
	}
}
