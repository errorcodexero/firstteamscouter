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
public class FTPFileUploader extends AsyncTask<File, Void, Void> {

    private final String user = "ftsscout", pwd = "ftsscouter";
    private final String remotePath = "./upload";
    private final byte serverIP[] = {(byte)10, (byte)0, (byte)0, (byte)100};

    public FTPFileUploader() {

    }

    @Override
    protected Void doInBackground(File... filesToSend) {
        FTPClient ftpClient = new FTPClient();
        boolean result = false;
        try {
            //ftpClient.connect(InetAddress.getByName(serverName));
            InetAddress ipv4 = Inet4Address.getByAddress(serverIP);
            ftpClient.connect(ipv4);
            result = ftpClient.login(user, pwd);
            Log.e("isFTPConnected", String.valueOf(result));
            ftpClient.changeWorkingDirectory(remotePath);

            if (ftpClient.getReplyString().contains("250")) {
                BufferedInputStream buffIn = null;
                ftpClient.setFileType(FTP.ASCII_FILE_TYPE);
                ftpClient.enterLocalPassiveMode();

                for(File fileToSend : filesToSend) {
                    buffIn = new BufferedInputStream(new FileInputStream(fileToSend));
                    //ProgressInputStream progressInput = new ProgressInputStream(buffIn, new Handler(Looper.myLooper()));
                    //result = ftpClient.storeFile(remotePath, progressInput);
                    result = ftpClient.storeFile(fileToSend.getAbsoluteFile().getName(), buffIn);
                    buffIn.close();

                    if(result) {

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
        }
        return null;
    }
}
