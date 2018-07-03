package callbacks

import org.argus.jawa.core.util.{MSet, msetEmpty}

class AndroidCallBacks {

  final val androidCallbacks: MSet[String] = msetEmpty
  def initAndroidCallbacks = {

    this.androidCallbacks += "android.accounts.OnAccountsUpdateListener"

    // android.animation
    this.androidCallbacks += "android.animation.Animator$AnimatorListener"
    this.androidCallbacks += "android.animation.LayoutTransition$TransitionListener"
    this.androidCallbacks += "android.animation.TimeAnimator$TimeListener"
    this.androidCallbacks += "android.animation.ValueAnimator$AnimatorUpdateListener"
    // android.app
    this.androidCallbacks += "android.app.ActionBar$OnMenuVisibilityListener"
    this.androidCallbacks += "android.app.ActionBar$OnNavigationListener"
    this.androidCallbacks += "android.app.ActionBar$TabListener"
    this.androidCallbacks += "android.app.Application$ActivityLifecycleCallbacks"
    this.androidCallbacks += "android.app.DatePickerDialog$OnDateSetListener"
    this.androidCallbacks += "android.app.FragmentBreadCrumbs$OnBreadCrumbClickListener"
    this.androidCallbacks += "android.app.FragmentManager$OnBackStackChangedListener"
    this.androidCallbacks += "android.app.KeyguardManager$OnKeyguardExitResult"
    this.androidCallbacks += "android.app.LoaderManager$LoaderCallbacks"
    this.androidCallbacks += "android.app.PendingIntent$OnFinished"
    this.androidCallbacks += "android.app.SearchManager$OnCancelListener"
    this.androidCallbacks += "android.app.SearchManager$OnDismissListener"
    this.androidCallbacks += "android.app.TimePickerDialog$OnTimeSetListener"
    // android.bluetooth
    this.androidCallbacks += "android.bluetooth.BluetoothProfile$ServiceListener"
    // android.content
    this.androidCallbacks += "android.content.ClipboardManager$OnPrimaryClipChangedListener"
    this.androidCallbacks += "android.content.ComponentCallbacks"
    this.androidCallbacks += "android.content.ComponentCallbacks2"
    this.androidCallbacks += "android.content.DialogInterface$OnCancelListener"
    this.androidCallbacks += "android.content.DialogInterface$OnClickListener"
    this.androidCallbacks += "android.content.DialogInterface$OnDismissListener"
    this.androidCallbacks += "android.content.DialogInterface$OnKeyListener"
    this.androidCallbacks += "android.content.DialogInterface$OnMultiChoiceClickListener"
    this.androidCallbacks += "android.content.DialogInterface$OnShowListener"
    this.androidCallbacks += "android.content.IntentSender$OnFinished"
    this.androidCallbacks += "android.content.Loader$OnLoadCanceledListener"
    this.androidCallbacks += "android.content.Loader$OnLoadCompleteListener"
    this.androidCallbacks += "android.content.SharedPreferences$OnSharedPreferenceChangeListener"
    this.androidCallbacks += "android.content.SyncStatusObserver"
    // android.database.Cursor
    this.androidCallbacks += "android.database.Cursor"  // sankar added
    // android.database.sqlite
    this.androidCallbacks += "android.database.sqlite.SQLiteTransactionListener"
    // android.drm
    this.androidCallbacks += "android.drm.DrmManagerClient$OnErrorListener"
    this.androidCallbacks += "android.drm.DrmManagerClient$OnEventListener"
    this.androidCallbacks += "android.drm.DrmManagerClient$OnInfoListener"
    // android.gesture
    this.androidCallbacks += "android.gesture.GestureOverlayView$OnGestureListener"
    this.androidCallbacks += "android.gesture.GestureOverlayView$OnGesturePerformedListener"
    this.androidCallbacks += "android.gesture.GestureOverlayView$OnGesturingListener"
    // android.graphics
    this.androidCallbacks += "android.graphics.SurfaceTexture$OnFrameAvailableListener"
    // android.hardware
    this.androidCallbacks += "android.hardware.Camera$AutoFocusCallback"
    this.androidCallbacks += "android.hardware.Camera$AutoFocusMoveCallback"
    this.androidCallbacks += "android.hardware.Camera$ErrorCallback"
    this.androidCallbacks += "android.hardware.Camera$FaceDetectionListener"
    this.androidCallbacks += "android.hardware.Camera$OnZoomChangeListener"
    this.androidCallbacks += "android.hardware.Camera$PictureCallback"
    this.androidCallbacks += "android.hardware.Camera$PreviewCallback"
    this.androidCallbacks += "android.hardware.Camera$ShutterCallback"
    this.androidCallbacks += "android.hardware.SensorEventListener"
    // android.hardware.display
    this.androidCallbacks += "android.hardware.display.DisplayManager$DisplayListener"
    // android.hardware.input
    this.androidCallbacks += "android.hardware.input.InputManager$InputDeviceListener"
    // android.inputmethodservice
    this.androidCallbacks += "android.inputmethodservice.KeyboardView$OnKeyboardActionListener"
    // android.location
    this.androidCallbacks += "android.location.GpsStatus$Listener"
    this.androidCallbacks += "android.location.GpsStatus$NmeaListener"
    this.androidCallbacks += "android.location.LocationListener"
    // android.media
    this.androidCallbacks += "android.media.AudioManager$OnAudioFocusChangeListener"
    this.androidCallbacks += "android.media.AudioRecord$OnRecordPositionUpdateListener"
    this.androidCallbacks += "android.media.AudioRecord$OnRecordPositionUpdateListener"
    this.androidCallbacks += "android.media.JetPlayer$OnJetEventListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnBufferingUpdateListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnCompletionListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnErrorListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnInfoListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnPreparedListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnSeekCompleteListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnTimedTextListener"
    this.androidCallbacks += "android.media.MediaPlayer$OnVideoSizeChangedListener"
    this.androidCallbacks += "android.media.MediaRecorder$OnErrorListener"
    this.androidCallbacks += "android.media.MediaRecorder$OnInfoListener"
    this.androidCallbacks += "android.media.MediaScannerConnection$MediaScannerConnectionClient"
    this.androidCallbacks += "android.media.MediaScannerConnection$OnScanCompletedListener"
    this.androidCallbacks += "android.media.SoundPool$OnLoadCompleteListener"
    // android.media.audiofx
    this.androidCallbacks += "android.media.audiofx.AudioEffect$OnControlStatusChangeListener"
    this.androidCallbacks += "android.media.audiofx.AudioEffect$OnEnableStatusChangeListener"
    this.androidCallbacks += "android.media.audiofx.BassBoost$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.EnvironmentalReverb$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.Equalizer$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.PresetReverb$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.Virtualizer$OnParameterChangeListener"
    this.androidCallbacks += "android.media.audiofx.Visualizer$OnDataCaptureListener"
    // android.media.effect
    this.androidCallbacks += "android.media.effect.EffectUpdateListener"
    // android.net.nsd
    this.androidCallbacks += "android.net.nsd.NsdManager$DiscoveryListener"
    this.androidCallbacks += "android.net.nsd.NsdManager$RegistrationListener"
    this.androidCallbacks += "android.net.nsd.NsdManager$ResolveListener"
    // android.net.sip
    this.androidCallbacks += "android.net.sip.SipRegistrationListener"
    // android.net.wifi.p2p
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ActionListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ChannelListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ConnectionInfoListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$DnsSdServiceResponseListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$DnsSdTxtRecordListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$GroupInfoListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$PeerListListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$ServiceResponseListener"
    this.androidCallbacks += "android.net.wifi.p2p.WifiP2pManager$UpnpServiceResponseListener"
    // android.os
    this.androidCallbacks += "android.os.CancellationSignal$OnCancelListener"
    this.androidCallbacks += "android.os.IBinder$DeathRecipient"
    this.androidCallbacks += "android.os.MessageQueue$IdleHandler"
    this.androidCallbacks += "android.os.RecoverySystem$ProgressListener"
    // android.preference
    this.androidCallbacks += "android.preference.Preference$OnPreferenceChangeListener"
    this.androidCallbacks += "android.preference.Preference$OnPreferenceClickListener"
    this.androidCallbacks += "android.preference.PreferenceFragment$OnPreferenceStartFragmentCallback"
    this.androidCallbacks += "android.preference.PreferenceManager$OnActivityDestroyListener"
    this.androidCallbacks += "android.preference.PreferenceManager$OnActivityResultListener"
    this.androidCallbacks += "android.preference.PreferenceManager$OnActivityStopListener"
    // android.security
    this.androidCallbacks += "android.security.KeyChainAliasCallback"
    // android.speech
    this.androidCallbacks += "android.speech.RecognitionListener"
    // android.speech.tts
    this.androidCallbacks += "android.speech.tts.TextToSpeech$OnInitListener"
    this.androidCallbacks += "android.speech.tts.TextToSpeech$OnUtteranceCompletedListener"
    // android.support - omitted
    // android.view
    this.androidCallbacks += "android.view.ActionMode$Callback"
    this.androidCallbacks += "android.view.ActionProvider$VisibilityListener"
    this.androidCallbacks += "android.view.GestureDetector$OnDoubleTapListener"
    this.androidCallbacks += "android.view.GestureDetector$OnGestureListener"
    this.androidCallbacks += "android.view.InputQueue$Callback"
    this.androidCallbacks += "android.view.KeyEvent$Callback"
    this.androidCallbacks += "android.view.MenuItem$OnActionExpandListener"
    this.androidCallbacks += "android.view.MenuItem$OnMenuItemClickListener"
    this.androidCallbacks += "android.view.ScaleGestureDetector$OnScaleGestureListener"
    this.androidCallbacks += "android.view.SurfaceHolder$Callback"
    this.androidCallbacks += "android.view.SurfaceHolder$Callback2"
    this.androidCallbacks += "android.view.TextureView$SurfaceTextureListener"
    this.androidCallbacks += "android.view.View$OnAttachStateChangeListener"
    this.androidCallbacks += "android.view.View$OnClickListener"
    this.androidCallbacks += "android.view.View$OnCreateContextMenuListener"
    this.androidCallbacks += "android.view.View$OnDragListener"
    this.androidCallbacks += "android.view.View$OnFocusChangeListener"
    this.androidCallbacks += "android.view.View$OnGenericMotionListener"
    this.androidCallbacks += "android.view.View$OnHoverListener"
    this.androidCallbacks += "android.view.View$OnKeyListener"
    this.androidCallbacks += "android.view.View$OnLayoutChangeListener"
    this.androidCallbacks += "android.view.View$OnLongClickListener"
    this.androidCallbacks += "android.view.View$OnSystemUiVisibilityChangeListener"
    this.androidCallbacks += "android.view.View$OnTouchListener"
    this.androidCallbacks += "android.view.ViewGroup$OnHierarchyChangeListener"
    this.androidCallbacks += "android.view.ViewStub$OnInflateListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnDrawListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnGlobalFocusChangeListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnGlobalLayoutListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnPreDrawListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnScrollChangedListener"
    this.androidCallbacks += "android.view.ViewTreeObserver$OnTouchModeChangeListener"
    // android.view.accessibility
    this.androidCallbacks += "android.view.accessibility.AccessibilityManager$AccessibilityStateChangeListener"
    // android.view.animation
    this.androidCallbacks += "android.view.animation.Animation$AnimationListener"
    // android.view.inputmethod
    this.androidCallbacks += "android.view.inputmethod.InputMethod$SessionCallback"
    this.androidCallbacks += "android.view.inputmethod.InputMethodSession$EventCallback"
    // android.view.textservice
    this.androidCallbacks += "android.view.textservice.SpellCheckerSession$SpellCheckerSessionListener"
    // android.webkit.DownloadListener
    this.androidCallbacks += "android.webkit.DownloadListener"
    // android.webkit.WebViewClient
    this.androidCallbacks += "android.webkit.WebViewClient"

    // android.widget
    this.androidCallbacks += "android.widget.AbsListView$MultiChoiceModeListener"
    this.androidCallbacks += "android.widget.AbsListView$OnScrollListener"
    this.androidCallbacks += "android.widget.AbsListView$RecyclerListener"
    this.androidCallbacks += "android.widget.AdapterView$OnItemClickListener"
    this.androidCallbacks += "android.widget.AdapterView$OnItemLongClickListener"
    this.androidCallbacks += "android.widget.AdapterView$OnItemSelectedListener"
    this.androidCallbacks += "android.widget.AutoCompleteTextView$OnDismissListener"
    this.androidCallbacks += "android.widget.CalendarView$OnDateChangeListener"
    this.androidCallbacks += "android.widget.Chronometer$OnChronometerTickListener"
    this.androidCallbacks += "android.widget.CompoundButton$OnCheckedChangeListener"
    this.androidCallbacks += "android.widget.DatePicker$OnDateChangedListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnChildClickListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnGroupClickListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnGroupCollapseListener"
    this.androidCallbacks += "android.widget.ExpandableListView$OnGroupExpandListener"
    this.androidCallbacks += "android.widget.Filter$FilterListener"
    this.androidCallbacks += "android.widget.NumberPicker$OnScrollListener"
    this.androidCallbacks += "android.widget.NumberPicker$OnValueChangeListener"
    this.androidCallbacks += "android.widget.PopupMenu$OnDismissListener"
    this.androidCallbacks += "android.widget.PopupMenu$OnMenuItemClickListener"
    this.androidCallbacks += "android.widget.PopupWindow$OnDismissListener"
    this.androidCallbacks += "android.widget.RadioGroup$OnCheckedChangeListener"
    this.androidCallbacks += "android.widget.RatingBar$OnRatingBarChangeListener"
    this.androidCallbacks += "android.widget.SearchView$OnCloseListener"
    this.androidCallbacks += "android.widget.SearchView$OnQueryTextListener"
    this.androidCallbacks += "android.widget.SearchView$OnSuggestionListener"
    this.androidCallbacks += "android.widget.SeekBar$OnSeekBarChangeListener"
    this.androidCallbacks += "android.widget.ShareActionProvider$OnShareTargetSelectedListener"
    this.androidCallbacks += "android.widget.SlidingDrawer$OnDrawerCloseListener"
    this.androidCallbacks += "android.widget.SlidingDrawer$OnDrawerOpenListener"
    this.androidCallbacks += "android.widget.SlidingDrawer$OnDrawerScrollListener"
    this.androidCallbacks += "android.widget.TabHost$OnTabChangeListener"
    this.androidCallbacks += "android.widget.TextView$OnEditorActionListener"
    this.androidCallbacks += "android.widget.TimePicker$OnTimeChangedListener"
    this.androidCallbacks += "android.widget.ZoomButtonsController$OnZoomListener"

    // bluetooth
    this.androidCallbacks += "android.bluetooth.BluetoothAdapter$LeScanCallback"
    this.androidCallbacks += "android.bluetooth.BluetoothProfile$ServiceListener"

    // nfc
    this.androidCallbacks += "android.nfc.NfcAdapter$CreateNdefMessageCallback"
    this.androidCallbacks += "android.nfc.NfcAdapter$OnNdefPushCompleteCallback"

    // recyclerview
    this.androidCallbacks += "android.support.v7.widget.RecyclerView$Adapter"
  }
}
