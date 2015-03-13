package com.wilsonvillerobotics.firstteamscouter.utilities;


import android.os.AsyncTask;
import android.util.Log;

import com.wilsonvillerobotics.firstteamscouter.FIRSTTeamScouter;

import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPClient;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;

/**
 * Created by TomS on 3/11/2015.
 *
 * http://stackoverflow.com/questions/7202987/uploading-a-file-from-android-to-a-server-through-ftp
 */
public class FTPFileUploader extends AsyncTask<File, Void, Boolean> {

    private String userName;
    private String password;
    private String remotePath;
    private byte serverIP[];

    public FTPFileUploader() {
        this.userName = FTSUtilities.user;
        this.password = FTSUtilities.pwd;
        this.remotePath = FTSUtilities.remoteUploadPath;
        this.serverIP = FTSUtilities.defaultServerIP;
    }

    @Override
    protected Boolean doInBackground(File... filesToSend) {
        FTPClient ftpClient = new FTPClient();
        boolean result = false;
        boolean backupSuccess = false;
        try {
            //ftpClient.connect(InetAddress.getByName(serverName));
            InetAddress ipv4 = Inet4Address.getByAddress(serverIP);
            ftpClient.connect(ipv4);
            result = ftpClient.login(userName, password);
            Log.e("isFTPConnected", String.valueOf(result));
            ftpClient.changeWorkingDirectory(remotePath);

            if (ftpClient.getReplyString().contains("250")) {
                BufferedInputStream buffIn = null;
                ftpClient.setFileType(FTP.ASCII_FILE_TYPE);
                ftpClient.enterLocalPassiveMode();

                for(File fileToSend : filesToSend) {
                    buffIn = new BufferedInputStream(new FileInputStream(fileToSend));
                    //ProgressInputStream progressInput = new ProgressInputStream(buffIn, new Handler(Looper.myLooper()));
                    //result = ftpClient.storeFile(remoteUploadPath, progressInput);
                    String fName = fileToSend.getAbsoluteFile().getName();
                    result = ftpClient.storeFile(fName, buffIn);
                    buffIn.close();

                    if(result) {
                        File backupDir = FTSUtilities.getFileDirectory("exported");
                        if (!backupDir.exists()) {
                            backupDir.mkdirs();
                        }

                        File backupFile = new File(backupDir, fName);
                        backupSuccess |= fileToSend.renameTo(backupFile);
                    }
                }
                ftpClient.logout();
                ftpClient.disconnect();
            }

        } catch (SocketException e) {
            Log.e(FIRSTTeamScouter.TAG, e.getStackTrace().toString());
        } catch (UnknownHostException e) {
            Log.e(FIRSTTeamScouter.TAG, e.getStackTrace().toString());
        } catch (IOException e) {
            Log.e(FIRSTTeamScouter.TAG, e.getStackTrace().toString());
        } catch (Exception e) {
            Log.e(FIRSTTeamScouter.TAG, e.getStackTrace().toString());
        }
        return result && backupSuccess;
    }
}
