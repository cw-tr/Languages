# AutoHotkey (AHK)

## Özet
AutoHotkey (AHK); 2003 yılında Chris Mallett tarafından icat edilen, Açık kaynaklı (Open-source) ve SADECE **Windows İşletim Sistemi** üzerinde çalışan; "Klavyedeki tuşlara ve Farenin tıklamalarına Hükmetmek, kısayollar (Hotkeys) yaratmak ve Ekranda insanüstü hızlarda Otomasyonlar (Makrolar/Botlar) çizmek" için üretilmiş C++ tabanlı evrendeki en tuhaf, en efsanevi **Masaüstü Makro ve Otomasyon Betik Dilidir.**

## Nedir ve Ne İşe Yarar?
Diyelim ki her sabah işe geldiğinizde 3 tane Excel dosyası açıyor, bir web sitesine girip şifrenizi yazıyor ve sonra o dosyaları kopyalayıp USB belleğe alıyorsunuz (Bu işlem 5 dakikanızı Alır).

AutoHotkey Dedi Ki: "Neden Bir Amele Gibi Her Gün Aynı Fare Tıklamalarını Yapıyorsun? Bana ufacık Bir AutoHotkey (AHK) betiği yaz! **CTRL + SHIFT + Z** Tuşuna Bastığında, Ben Fareyi Milisaniyede O Excelerin Üstüne Götüreyim Tıklayayım, Sonra Siteyi Acip Şifreni 'Aşırı hızlı Bir Hayelet Klavuz(Typer)' gibi yazayım!". İşlemi O(AHK) Yaparsa 3 Saniye Sürer!

**Ne İşe Yarar?**
* **Windows Kısayol (Hotkey) Ustası:** İsterseniz Bilgisayardaki "Caps Lock" tuşunu İptal edip Onu "Ses Kapatma (Mute)" tuşuna anında Çevirebilirsiniz. Veya F12'ye Basınca Direk Photoshop'u actırtıblirirsiniz.
* **Oyun Botları (Macro/Cheat):** World of Warcraft veya Metin2 gibi oyunlarda; "Adamın CANI(Kırmızı RenkPikseli) yüzde 50'nin altına düşürse GİT VE OTOMATİK 3 NUMARALI TUSA(Can İksiri) BAS!" Diyen İnanılmaz Makroların Yazıldığı İllegale de Kayan O dildir (MMORPG oyuncuları Buna TApAR!).
* **Veri Girişi (Data Entry) Uzmanları:** Müşteri Hizmetlierinde Çalşıan adam; Her gun Cok Uzzzzun Bir Yaziyi YazmskYerine; Sadece `!mrhba` yzazar, AutoHoıtky O kelmyiii Siler Veee "Sayin Müşterisizmirz BIzi TEtcih Etihnjzic Innn Teskkleree!!" Diye 10 satrlikk metnizi SAaniiyesinde Yapıştıraar. (Text Expansion).

## Dilin Mantığı ve Kod Yapısı
Tam bir Jargondur (C syntax'ından Bozuk olarak Cıikmis VBScript Kokan Bir Yapisidir). Modernliği/Sıfırdir Ama Çalışma İştihamı 100/100 dürr. 
Son yıllarda **AHK Version 2** çıkmış Ve Dili biraz daha "Nesne Yöenelimlii" ve Düzgün(Mantıiklı) hale GEetirmiştiri, Fakkat Esksi Jenerasoy(v1) Hala İTnernettkeşi Her yerde GEziz.

Kısayllar Karakterlrle BAggnalii:: 
`^` = CTRL 
`!` = ALT 
`+` = SHIFT 
`#` = Windows Tuşu


### Örnek Bir AutoHotkey Kodu: Kısayollar ve Pİksel Okuyan Oyuncu Botu (Macro):
Aşağıda Bir AHK Betiği Görüyouruzz. BU dosya(.ahk) ya CiFrt Tiklandigindiaa Arkaplanda (Tray icon Olaral)gizilince Çalışırve Dinlemelyre Beşlaır:

```autohotkey
; BU BİR AUTOHOTKEY KODUDUR (Noktali virgul yorum satiridir)
#Requires AutoHotkey v2.0 ; AHK versyon 2 Geregini koyyoytuz (MOdern SOldizszjii)

;-----------------------------------------------------------------------
; OZELLIK 1 : METIN GENISLETICI (TEXT EXPANSION)
; Eger Kullanici klavyedn "@@" Yazzrve Boşulağa Basssa, AHK Onu Silip Asagidiaski Uzunn MaiLli Yapistir!!

::@@::info@benimsirketimmudurluguisleri.com


;-----------------------------------------------------------------------
; OZELLIK 2  : OZEL KISAYOL TUSU!!
; CTRL + ALT + N (^!n) Tusuna Basildifinfdaa NOTEPAD (HeSspMkajnesnsi i) AAAAACCCTT !

^!n:: {
    Run "notepad.exe"   ; Windowsun ExenSini Calsitirr
    WinWaitActive "Untitled - Notepad"  ; O Pencere acilililana AKdar BEKELEE!! (Yoksa kod kacar hizlanilir)
    Send "AHK Cok Yasa!! Ben Haylaeet  Ellermile Yaziyorumuuuhhh!" ; Klavyeeen Usta CAna Vuruslar Yaaaap!
}


;-----------------------------------------------------------------------
; OZELLIK 3 : EKRANDA PIKSEL TARAMASI (Oyunlardiaki AIMBOT VeYA Heahllth Potion BOOTuuu!)
; F8 Tusuna Baseinciia CaLSIISir: 

F8:: {
    ; EKRANDA (X:400, Y:300) Noktasindaki PKSSELIN RENGINIII OGREENNN V E Al!
    renk := PixelGetColor(400, 300)
    
    ; Eger Oradaki Pikssel Rengg (0xFF0000 =KİMRİZİ/Caan dustiu !!Yaaaanii) Ise TUSLAA BAAass!
    if (renk == 0xFF0000) {
        
        Send "1"      ; Klazvueden '1' Turuua Bas (1ncie yeteengeee / Potion İcs!!)
        Sleep 500     ; Oyunun Motoru Cokkk Cok hizli Vrudugnumud Anllmsasin DiE Yarini SaniveBekle
        Click 500,500 ; Mauseun İmklenicini Orttya GötutrVe Sol Tıkallal!!!
    }
}
```

Bu kod 2 Kİlobyteliık(Ufanxcix) Bİr Programdıdr ArkaPşalnda Çaılısr. Mauseunuzn Kontrolünüü Kendi İrdasdsylnizle Bir "RObitaaa" TAaslim Edersiniz! AutoHotkey, Bir İşletimsiseMtimini(Windows) EğğiP Bükemnenn, Pencelereelrın(Hwnd) Titlellarrnii Yok edebilken "TanrisaaL Bir Hacker/Amelesliks" SİLashıdır.

## Kimler Kullanır?
* Evrenddeli, Süreskli Aynj Tuşlara (Ecxell C/p , Fotoasghop MAkaralori, Verı Gigiririysi) Baasan **Beyaz Yakalı (Ofissi)** Çalışanlariari İle Hayatının Zamaamindnan SAnaaiyeler KAZANMAK Isteyen **Verimlilik(PRoductivity) Hastslaarui**.
* Ve Tarihteiik BÜtün (Cok Gelşsimisi Olmaynyajn/MEmomory Okyuamyan) Oynuncuur **MMORPG Faming (Tarimcia/Grind) Boptlaihini**  AhK iLe yazallalrar... Zira İnsasnnıjn Gozunun Yorulup Ekranddan Gödreuuh Renkkleri, AHK Saniyeneden Milihyopon Kere Taarappı Tepki Veeeririi. Windowsunuun En gizli VRe Karanlkk Kutyuusdurur.
