# AutoHotkey (AHK)

## Özet
AutoHotkey (AHK); 2003 yılında Chris Mallett tarafından icat edilen, Açık kaynaklı (Open-source) ve SADECE **Windows İşletim Sistemi** üzerinde çalışan; "Klavyedeki tuşlara ve Farenin tıklamalarına Hükmetmek, kısayollar (Hotkeys) yaratmak ve Ekranda insanüstü hızlarda Otomasyonlar (Makrolar/Botlar) çizmek" için üretilmiş C++ tabanlı evrendeki en tuhaf, en efsanevi **Masaüstü Makro ve Otomasyon Betik Dilidir.**

## Nedir ve Ne İşe Yarar?
Diyelim ki her sabah işe geldiğinizde 3 tane Excel dosyası açıyor, bir web sitesine girip şifrenizi yazıyor ve sonra o dosyaları kopyalayıp USB belleğe alıyorsunuz (Bu işlem 5 dakikanızı Alır).

AutoHotkey Dedi Ki: "Neden Bir Amele Gibi Her Gün Aynı Fare Tıklamalarını Yapıyorsun? Bana ufacık Bir AutoHotkey (AHK) betiği yaz! **CTRL + SHIFT + Z** Tuşuna Bastığında, Ben Fareyi Milisaniyede O Excelerin Üstüne Götüreyim Tıklayayım, Sonra Siteyi Açıp Şifreni 'Aşırı hızlı Bir Hayalet Kılavuz (Typer)' gibi yazayım!". İşlemi O (AHK) Yaparsa 3 Saniye Sürer!

**Ne İşe Yarar?**
* **Windows Kısayol (Hotkey) Ustası:** İsterseniz Bilgisayardaki "Caps Lock" tuşunu İptal edip Onu "Ses Kapatma (Mute)" tuşuna anında Çevirebilirsiniz. Veya F12'ye Basınca Direkt Photoshop'u açtırtabilirsiniz.
* **Oyun Botları (Macro/Cheat):** World of Warcraft veya Metin2 gibi oyunlarda; "Adamın CANI (Kırmızı Renk Pikseli) yüzde 50'nin altına düşerse GİT VE OTOMATİK 3 NUMARALI TUŞA (Can İksiri) BAS!" Diyen İnanılmaz Makroların Yazıldığı İllegale de Kayan O dildir (MMORPG oyuncuları Buna TAPAR!).
* **Veri Girişi (Data Entry) Uzmanları:** Müşteri Hizmetlerinde Çalışan adam; Her gün Çok Uzzzzun Bir Yazıyı Yazmak Yerine; Sadece `!mrhba` yazar, AutoHotkey O kelimeyi Siler Veee "Sayın Müşterimiz Bizi Tercih Ettiğiniz İçin Teşekkürler!!" Diye 10 satırlık metninizi Saniyesinde Yapıştırır. (Text Expansion).

## Dilin Mantığı ve Kod Yapısı
Tam bir Jargondur (C syntax'ından Bozuk olarak Çıkmış VBScript Kokan Bir Yapısıdır). Modernliği Sıfırdır Ama Çalışma İştahı 100/100'dür. 
Son yıllarda **AHK Version 2** çıkmış Ve Dili biraz daha "Nesne Yönelimli" ve Düzgün (Mantıklı) hale Getirmiştir, Fakat Eski Jenerasyon (v1) Hala İnternetteki Her yerde Gezer.

Kısayollar Karakterlerle Bağlantılı: 
`^` = CTRL 
`!` = ALT 
`+` = SHIFT 
`#` = Windows Tuşu


### Örnek Bir AutoHotkey Kodu: Kısayollar ve Piksel Okuyan Oyuncu Botu (Macro):
Aşağıda Bir AHK Betiği Görüyoruz. Bu dosya (.ahk) ya Çift tıklandığında Arka Planda (Tray icon olarak) gizlice Çalışır ve Dinlemeye Başlar:

```autohotkey
; BU BİR AUTOHOTKEY KODUDUR (Noktalı virgül yorum satırıdır)
#Requires AutoHotkey v2.0 ; AHK versiyon 2 gereğini koyuyoruz (Modern Söz Dizimi)

;-----------------------------------------------------------------------
; ÖZELLİK 1: METİN GENİŞLETİCİ (TEXT EXPANSION)
; Eğer Kullanıcı klavyeden "@@" Yazar ve Boşluğa Basarsa, AHK Onu Silip Aşağıdaki Uzun Maili Yapıştırır!!

::@@::info@benimsirketimmudurluguisleri.com


;-----------------------------------------------------------------------
; ÖZELLİK 2: ÖZEL KISAYOL TUŞU!!
; CTRL + ALT + N (^!n) Tuşuna Basıldığında NOTEPAD (Not Defteri) AÇILSIN!

^!n:: {
    Run "notepad.exe"   ; Windowsun Exesini Çalıştırır
    WinWaitActive "Untitled - Notepad"  ; O Pencere açılana Kadar BEKLE!! (Yoksa kod kaçar hızlanılır)
    Send "AHK Çok Yaşa!! Ben Hayalet Ellerimle Yazıyorum!" ; Klavyeden Usta Cana Vuruşlar Yap!
}


;-----------------------------------------------------------------------
; ÖZELLİK 3: EKRANDA PİKSEL TARAMASI (Oyunlardaki AIMBOT VEYA Health Potion BOTU!)
; F8 Tuşuna Basınca ÇALIŞIR: 

F8:: {
    ; EKRANDA (X:400, Y:300) Noktasındaki PİKSELİN RENGİNİ ÖĞREN V E Al!
    renk := PixelGetColor(400, 300)
    
    ; Eğer Oradaki Piksel Rengi (0xFF0000 = KIRMIZI / Can düştü!! Yani) İse TUŞA BAS!
    if (renk == 0xFF0000) {
        
        Send "1"      ; Klavyeden '1' Tuşuna Bas (1. yeteneği / Potion İç!!)
        Sleep 500     ; Oyunun Motoru Çok Çok hızlı vurduğumuzu anlamasın diye yarım saniye bekle
        Click 500, 500 ; Mouse'un İmlecini Ortaya Götür ve Sol Tıkla!!!
    }
}
```

Bu kod 2 Kilobaytlık (Ufacık) Bir Programdır, Arka Planda Çalışır. Mouse'unuzun Kontrolünü Kendi İradenizle Bir "Robota" Teslim Edersiniz! AutoHotkey, Bir İşletim Sistemini (Windows) Eğip Bükebilen, Pencerelerin (Hwnd) Başlıklarını Yok edebilen "Tanrısal Bir Hacker/Ameleslik" Silahıdır.

## Kimler Kullanır?
* Evrendeki Sürekli Aynı Tuşlara (Excel C/P, Photoshop Makroları, Veri Girişi) Basan **Beyaz Yakalı (Ofis)** Çalışanları İle Hayatının Zamanından Saniyeler KAZANMAK İsteyen **Verimlilik (Productivity) Hastaları**.
* Ve Tarihteki Bütün (Çok Gelişmiş Olmayan/Memory Okumayan) Oyuncular **MMORPG Farming (Tarımcı/Grind) Botlarını** AHK ile yazarlar... Zira İnsanın Gözünün Yorulup Ekrandan Gördüğü Renkleri, AHK Saniyede Milyon Kere Tarayıp Tepki Verir. Windows'un En gizli Ve Karanlık Kutusudur.
