package com.wilsonvillerobotics.firstteamscouter;

import android.graphics.Bitmap;

/**
 * Created by TomS on 2/15/2015.
 * http://javatechig.com/android/android-gridview-example-building-image-gallery-in-android
 */
public class ImageItem {
    private Bitmap image;
    private String title;

    public ImageItem(Bitmap image, String title) {
        super();
        this.image = image;
        this.title = title;
    }

    public Bitmap getImage() {
        return image;
    }

    public void setImage(Bitmap image) {
        this.image = image;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }
}