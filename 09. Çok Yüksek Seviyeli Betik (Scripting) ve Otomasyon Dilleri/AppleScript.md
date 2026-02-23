# AppleScript

## Özet
AppleScript; 1993 yılında Apple tarafından Mac OS (Classic ve Modern macOS) işletim sisteminin devasa pencereli uygulamalarında, Programcılardan (C/Objective-C bilenlerden) ziyade **"Senaryo (Script) yazan sıradan şirket içi kullanıcıların ve grafikerlerin"** Fare(Mouse) ile yapacakları eylemleri sanki Birbiriyle konuşan İnsan İngilizcesi komutları atarak (Makro/Otomasyon) yapması için üretilen aşırı-İngilizce ve doğal akışlı betik dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda Mac (Apple) işletim sistemi matbaalar, grafik tasarımcılar ve müzisyenlerin bir numaralı bilgisayarıydı. Bir Grafiker her sabah işe geldiğinde Photoshop'u açıp, "Bütün görselleri %50 küçült, sonra masaüstündeki bir klasöre Taşı, oradan Email uygulamasını aç ve patrona Yolla!" diye amelelik yapıyordu.

Apple, "Application Sözlükleri (AppleEvents)" adı verilen bir teknoloji çıkardı. Kendi programlarının (Finder, Safari, Mail, Photoshop) kalbine dinleyici kancalar taktı. AppleScript de kelimenin tam anlamıyla "İngilizce Düz Yazı" yazarak sisteme şöyle emredebilmenin yoluydu: *Tell application "Finder" to open folder "Fotograflar"*. (Gidip Finder programına söyle de Fotoğraflarımı bir açsın hele!). Okuması C'den çok düz lise makalesine benzer.

**Ne İşe Yarar?**
* **Mac Kullanıcı Otomasyonları (Makrolar):** Automator veya yeni adıyla Shortcuts(Kısayollar) uygulamasının arka plan eylemlerinin en kompleks versiyonları. iTunes(Apple Music)'u aç, sıradaki şarkıya geç ve şarkının ismini Twitter/Mesaj olarak at! gibi GUI (Grafik) programları bir nevi arka planda "Cereyanla / Kukla İpleriyle" birbirine bağlayam araçtır.
* Yayıncılık (Publishing/Print) ve Medya sektörlerinde, (Geliştirici veya yazılımcı olmayan sıradan kreatif editörlerin) iş akışı (Workflow) borusu olarak kullanılır.

## Dilin Mantığı ve Kod Yapısı
AppleScript, "Doğal Dil Programlama (Natural Language Programming)" felsefesinin en garip çocuklarından biridir (Logo ve SQL ile birlikte). Amaç İngilizce konuşuyor hissiyatı yaratmaktır. 
Döngüler `for x in` değil; İngilizcedeki tekrarlama kelimesi **`repeat`** kelimesi ile açılır. Parantezler çok nadir kullanılır, Cümle gibi bağlaç (to, of, in) zengindir.

Bütün otomasyonlar **`tell ... end tell` (Söyle... Söylemeyi Bitir)** bloklarının içine gömülüdür. Çünkü sistem "Kime ne emredeceğini" AppleEvent aracılığıyla bilmek ister. 

**Örnek İşleyiş (Sembolik Olarak):**
Python: `os.system('osascript -e ...')` (C işletim sistemi bağılılığı).
AppleScript: `tell application "Safari" to activate` (Safari uygulamasını atla (Kuklaya) ve aktif et(Öne getir)).

### Örnek Bir AppleScript Kodu: Safariyi Açıp Mac Masaüstüne Pop-Up Uyarısı Attırmak
Tipik bir Mac OS Kullanıcısının (Visual Code / Terminal yerine Apple'ın gömülü "Script Editor" uygulamasına yazdığı) o hikaye anlatan kurgu (Tell bloğu):

```applescript
-- AppleScript Yorum Satirlari Iki Tire (--) İle gosterilir (Veya Hashtag # kabul ediir)

-- 1. DEĞİŞKEN (STRING) ATAMALARI
-- 'Set (Ayarla)' kelimesi yine VB'deki gibi basimizda (Tiptesiz bir dildir, Varianttir)
set karsilamaMesaji to "Sayin Tasarimci, Sabah Mesainiz Basliyor!"
set donguSayisi to 3


-- 2. DÖNDÜ VE SESLI OKUMA (Bir İnsanla Fısıltıgibi konuşmak)
-- Donguler "Repeat [Sayi] times" Olarak C dilindeki o int'leri reddeder:
repeat donguSayisi times
    
    -- Mac İşletim Sisteminin kalbindeki SES(Text-to-Speech) Motorunu tetikle! (say)
    -- "Bip" (Beep) Sesi Cikart ve Siri'nin Sesiyle bunu Yüksek Sesle Oku:
    beep
    -- say "Sistem uyanıyor" -- (Eger yorumi cevirisenir Siri konusur)
    
end repeat


-- 3. KUKLALARI (UYGULAMALARI) İPİNDEN OYNATMA: TELL BLOKLARI

-- "System Events (Mac Os Cekirdegi)" ine Soyle ki: Benim ekrana Dialog Çizssin!
tell application "System Events"
    
    -- Pop up'in İcine (Mesajimiz degiskenini) koy, Baslik ekle , Ve İkonu Cark yap!
    display dialog karsilamaMesaji with title "Otomasyon Baslatildi" buttons {"Tamam, Kur", "Iptal Et"} default button "Tamam, Kur"
    
end tell


-- 4. KUKLA SAFARIYI AÇIP İNTERNET SİTESİNE GİT VE UYGULAMAYI KÜÇÜLT!
-- UYGULAMA 2 (Safari Tarayicisina Komut veriyoruz) 

tell application "Safari"
    
    -- Aktif et (Pencereyi one Firlattir!)
    activate
    
    -- Yeni Bir Sayfa (Document/Tab) yaratıp URl'sine Wikipedia bagla:
    make new document with properties {URL:"https://www.apple.com"}
    
end tell

-- Sonunda Her ikisine birden Emrettik ve bitt (Istersen Photoshop'u tell edip Firca buyutebilirsin)
```
Bu AppleScript Editor'ünden veya Mac Terminalinden `osascript dosya.scpt` (Derlenmiş script) olarak çalıştırıldığında işletim sisteminde şizofrenik bir şeyler olur. Fare kendiliğinden pencereleri açar, Safari zıplar ve siteye gider! Sistemler arası (Inter-Process Communication) eziyeti sadece "Söyle (Tell)" kelimesiyle parçalamıştır.

## Kimler Kullanır?
* Klasik bilgisayar kodu yazmayan ama Otomasyon bağımlısı olan, şirketlerinde yüzlerce PDF veya Fatura tasarlayan **Mac OS Tasarımcıları, Müzisyenleri, Editörleri ve Sekreterleri**.
* Mac OS Geliştiricileri (Objective C / Swift coderları) uygulamalarına AppleScript ile kontrol edilebilir "Sözlükler (Dictionaries)" verirler ki üçüncü partiler otomasyon yaratabilsin.
* GÜNÜMÜZDE; iOS'un (iPhone) da dahil olmasıyla Apple, bu işin sadece Mac'de kalan zor script kısmı yerine Her cihazda görsel (Pencere Sürükleyici) çalışabilen **Shortcuts (Kestirmeler)** uygulamasını entegre etmiş ve 2021 itibariyle AppleScript mimarisini biraz daha "Legacy / Sonlanan efsane" bloğuna doğru evriltmeye başlamıştır. Swift'e (SwiftAutomation) göçülmektedir.
