package com.wilsonvillerobotics.firstteamscouter.utilities;

import java.io.IOException;
import java.io.InputStream;

import android.os.Bundle;
import android.os.Handler;
import android.os.Message;

/**
 * Created by TomS on 3/11/2015.
 *
 * https://code.google.com/p/catroid/source/browse/catroid/src/at/tugraz/ist/catroid/web/ProgressInputStream.java?r=285acacb590bce41ef598237d1fd6fbe35a495c9
 */
public class ProgressInputStream extends InputStream {

    public static final String UPLOAD_PROGRESS_KEY = "uploadProgress";
    public static final int UPLOAD_REFRESH_MS = 300;

    private InputStream inputStream;
    private Handler handler;
    private long progress;
    private long updateTime;
    private boolean closed;
    private int totalSize;

    public ProgressInputStream(InputStream inputStream, Handler handler) {
        this.inputStream = inputStream;
        this.handler = handler;
        this.progress = 0;
        this.closed = false;
        updateTime = System.currentTimeMillis();
        try {
            this.totalSize = inputStream.available();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public int read(byte[] b, int off, int len) throws IOException {
        int count = inputStream.read(b, off, len);
        if (count > 0) {
            progress += count;
        }
        maybeUpdateDisplay(progress);
        return count;
    }

    private void maybeUpdateDisplay(long progress) {
        if (System.currentTimeMillis() - updateTime > UPLOAD_REFRESH_MS) {
            Message message = new Message();
            Bundle bundle = new Bundle();
            double progressPercent = (100 / (double) totalSize) * progress;
            bundle.putInt(UPLOAD_PROGRESS_KEY, (int) progressPercent);
            message.setData(bundle);
            handler.sendMessage(message);
            updateTime = System.currentTimeMillis();
        }
    }

    @Override
    public int read() throws IOException {
        int count = inputStream.read();
        return count;
    }

    @Override
    public void close() throws IOException {
        super.close();
        if (closed) {
            throw new IOException();
        }
        closed = true;
    }
}
