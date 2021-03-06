package com.wilsonvillerobotics.firstteamscouter;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.channels.FileChannel;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.CompetitionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.DBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.ImportTransactionsDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTPFileDownloader;
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

import org.apache.commons.net.ftp.FTPFile;

public class DataImportActivity extends Activity implements View.OnClickListener {

	protected static final int TIMER_RUNTIME = 10000; // in ms --> 10s

    private DBAdapter dbAdapter;
	private MatchDataDBAdapter matchDataDBAdapter;
	private TeamMatchDBAdapter teamMatchDBAdapter;
	private TeamDataDBAdapter teamDataDBAdapter;
    private CompetitionDataDBAdapter cdDBAdapter;
    private RobotDataDBAdapter robotDataDBAdapter;

    private TextView txtStatus;
	private TextView txtTestDataAlert;
	protected ProgressBar mProgressBar;
	protected String tabletID;
    private long competition_id;
	
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
        this.competition_id = intent.getLongExtra("competition_id", 0);
		
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
            dbAdapter = new DBAdapter(this);
			matchDataDBAdapter = new MatchDataDBAdapter(this);
			teamMatchDBAdapter = new TeamMatchDBAdapter(this);
			teamDataDBAdapter = new TeamDataDBAdapter(this);
            robotDataDBAdapter = new RobotDataDBAdapter(this);
            cdDBAdapter = new CompetitionDataDBAdapter(this);
		} catch(SQLException e) {
			e.printStackTrace();
			matchDataDBAdapter = null;
			teamMatchDBAdapter = null;
			teamDataDBAdapter = null;
            robotDataDBAdapter = null;
            cdDBAdapter = null;
		}
		
		String statusMessage = "";
		String testDataAlertMessage = "";
        Button btnImportTestData = (Button)findViewById(R.id.btnImportTestData);
		if(FTSUtilities.TEST_MODE) {
			statusMessage += "Press the Import Test Data button to import test data for " + numTestMatches + " teams\n";
			testDataAlertMessage = "TEST DATA MODE";
            if(btnImportTestData != null) btnImportTestData.setEnabled(true);
		} else {
            if(btnImportTestData != null) btnImportTestData.setEnabled(false);
		}

        statusMessage += "Press the 'Download Data' button to download data files from the laptop\n";
        statusMessage += "Once the download completes, press the Import Data to import the files";
        //"Expected format is:\nCompetition ID : Time : Match Type : Match Number : Red1 : Red2 : Red3 : Blue1 : Blue2 : Blue3";

        txtStatus = (TextView) findViewById(R.id.txtStatus);
	    if(txtStatus != null) txtStatus.setText(statusMessage);
	    
	    txtTestDataAlert = (TextView) findViewById(R.id.txtTestDataAlert);
        if(txtTestDataAlert != null) txtTestDataAlert.setText(testDataAlertMessage);
	    
	    mProgressBar = (ProgressBar)findViewById(R.id.progressBar1);
        configureButtons();
	}

    private void configureButtons() {
        Button btnImportTestMatchData = (Button)findViewById(R.id.btnImportTestData);
        if(btnImportTestMatchData != null) btnImportTestMatchData.setOnClickListener(this);

        Button btnImportFromXml = (Button)findViewById(R.id.btnImportFromXml);
        if(btnImportFromXml != null) btnImportFromXml.setOnClickListener(this);

        Button btnDownloadDataFiles = (Button)findViewById(R.id.btnDownloadDataFiles);
        if(btnDownloadDataFiles != null) btnDownloadDataFiles.setOnClickListener(this);

        Button btnDisplayMetrics = (Button)findViewById(R.id.btnDisplayMetrics);
        if(btnDisplayMetrics != null) btnDisplayMetrics.setOnClickListener(this);
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

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.btnImportTestData:
                btnImportTestDataOnClick();
                break;
            case R.id.btnImportFromXml:
                btnImportFromXmlOnClick();
                break;
            case R.id.btnDownloadDataFiles:
                btnDownloadFilesOnClick();
                break;
            case R.id.btnDisplayMetrics:
                btnDisplayScreenMetrics();
                break;
        }
    }

    private void btnImportTestDataOnClick() {
        String importStatusMessage = "Import Test Data Clicked";
        if (FTSUtilities.TEST_MODE) {
            try {
                dbAdapter.deleteTableData();
                cdDBAdapter.populateTestData();
                matchDataDBAdapter.populateTestData();
                teamDataDBAdapter.populateTestData();
                teamMatchDBAdapter.populateTestData();
                //teamMatchDBAdapter.populateTestData(competition_id, matchDataDBAdapter.populateTestData(numTestMatches), teamDataDBAdapter.populateTestData());
                importStatusMessage = "Test data import complete";
            } catch (Exception e) {
                FTSUtilities.printToConsole("ImportMatchDataActivity::btnOK.onClick : ERROR");
                importStatusMessage = "ERROR importing data: " + e.getMessage();
                e.printStackTrace();
            }
        }
        txtStatus.setText(importStatusMessage);
    }

    private void btnImportFromXmlOnClick() {
        FTSUtilities.printToConsole("ImportMatchDataActivity::btnImportFromXml.onClick");
        txtStatus.setText("Importing from XML");

        String filePrefix = "ftsData";
        File externalFilesDir = FTSUtilities.getFileDirectory("download");

        File[] xmlFileList = externalFilesDir.listFiles(new XmlDataFilenameFilter(filePrefix, ".xml"));

        // Expected file name pattern: ftsDataExport-table-timestamp.xml
        String storageState = Environment.getExternalStorageState();
        if (storageState.equals(Environment.MEDIA_MOUNTED)) {
            ImportTransactionsDBAdapter itDBAdapter = new ImportTransactionsDBAdapter(this);
            for (File f : xmlFileList) {
                if (f.exists() && f.isFile()) {
                    boolean fileNotYetImported = itDBAdapter.fileHasNotBeenImported(f.getAbsoluteFile().getAbsolutePath());
                    if(fileNotYetImported) {
                        String parts[] = f.getName().split("-");
                        String tableName = (parts.length > 1) ? parts[1] : "UNKNOWN-TABLE";

                        DBAdapter.TABLE_NAMES table = DBAdapter.TABLE_NAMES.getTableByTableName(tableName);

                        if (table != null) {
                            txtStatus.setText("File Found for table " + tableName + ", Import Commencing\n");
                            String firstColumn = DBAdapter.getFirstColumnName(table);
                            String lastColumn = DBAdapter.getLastColumnName(table);
                            String insertStatement = dbAdapter.getInsertStatementFromXmlTable(f, tableName, firstColumn, lastColumn);

                            if(!insertStatement.isEmpty()) {
                                txtStatus.setText(insertStatement);
                                boolean imported = dbAdapter.importRecords(insertStatement);
                                if (imported) {
                                    String filePath = f.getAbsoluteFile().getAbsolutePath();
                                    itDBAdapter.addImportedFile(filePath);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    private void btnDownloadFilesOnClick() {
        FTPFileDownloader ftpDL = new FTPFileDownloader();
        String stat = "";
        try {
            FTPFile[] files = ftpDL.execute().get();
            stat = "File(s) Downloaded:";
            for (FTPFile f : files) {
                if (f != null) stat += "\n\t" + f.getName();
            }
        } catch (NullPointerException npe) {
            FTSUtilities.printToConsole("No files downloaded via FTP");
            stat += " 0\n";
        } catch (Exception e) {
            e.printStackTrace();
        }
        txtStatus.setText(stat);
    }

    private void btnDisplayScreenMetrics() {
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

    public class XmlDataFilenameFilter implements FilenameFilter {
        String filePrefix;
        String ext;

        public XmlDataFilenameFilter(String filePrefix, String ext) {
            this.filePrefix = filePrefix;
            this.ext = ext;

        }

        @SuppressLint("DefaultLocale")
        @Override
        public boolean accept(File dir, String filename) {
            //If you want to perform a case-insensitive search
            boolean matches = filename.toLowerCase().startsWith(filePrefix.toLowerCase());
            matches &= filename.toLowerCase().endsWith(ext.toLowerCase());
            return matches;
        }
    }
}
