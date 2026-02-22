# PowerShell

## Özet
PowerShell; 2006 yılında Microsoft tarafından, Windows'un eski, çok kısıtlı ve ilkel "CMD (.bat/.cmd)" sisteminin yetersizliğini yok edip Linux/Bash gücünü geçmek için yaratılmış, tamamen Nesne Yönelimli (Object-Oriented) C# (.NET) ekosistemine bağlı muazzam güçlü İşletim Sistemi Otomasyon ve Görev Yöneticisi dilidir.

## Nedir ve Ne İşe Yarar?
1990'larda Linux dünyası "BASH" adındaki inanılmaz komut diliyle işletim sistemlerini otomasyona çevirip havada uçururken, Windows kullanıcıları siyah ve yeteneksiz `cmd.exe (Command Prompt)` ekranına hapsolmuşlardı. CMD ekranı yazıları sadece aptal bir `String (Metin)` yığını olarak okuyor, kelime parçalamaktan başka analiz yapamıyordu.

Microsoft, tamamen devrimsel olan kendi Kabuk (Shell) programını yazdı. En kritik farkı şuydu: PowerShell ekranda görünen yazıyı "düz metin/string" olarak tutmaz; arka planda o yazıyı devasa bir **.NET Objesine (Nesneye/Class'a)** hapseder. O listelediğiniz dosya aslında `C# File Object`'tir. Onun rengine tıpkı C# programlıyormuşsunuz gibi nesnenin içinden müdahale edersiniz.

**Ne İşe Yarar?**
* **Windows Sunucu / Active Directory Yönetimi:** Milyon dolarlık şirketlerin, 5 bin çalışana tek seferde hesap açma, eposta dağıtımı yetkilendirme ve Exchange sunucularındaki binlerce Windows ayarını (Registry) saniyeler içinde sadece `.ps1` uzantılı PowerShell komutlarıyla otomatikleştirmesi.
* **Hibrit ve Bulut Altyapı:** Azure (Microsoft) Cloud sunucularının yaratılması veya kapatılması, Windows Cihaz Güvenliği/Antivirüslerinin kurumsal dağıtımı tamamen PowerShell Scriptleri kullanılarak Cihaz Politika Kuralları ile fırlatılır. (Ayrıca "PowerShell Core" ile artık Linux ve macOS sistemlerinde bile nativ olarak çalışır hale gelmiştir).

## Dilin Mantığı ve Kod Yapısı
Çok belirgin, dünyadaki tüm dillerden farklı, aşırı "Sözlü, Kuralcı" bir "Borumlama (Cmdlet)" tarzı vardır.

Tüm ana fonksiyon adları (Cmdlet denir) kesinlikle bir **Fiil-İsim (Verb-Noun)** iskeletiyle yazılmak zorundadır:
- Oku = `Get-Content`
- Veri Üret = `New-Item`
- İşlemi Durdur = `Stop-Process`

Bu yüzden (Bash'in `ls`, `grep` gibi kısaltmalarına kıyasla) uzundur ancak inanılmaz anlaşılır ve İngilizce okur gibi çözülür. Daha önceden bahsettiğim gibi Değişkenler `$` ile döner ancak Linux'taki gibi Cümleleri `(sed / awk)` ile regex'e parçalamak eziyeti bitmiştir. Objenin ismini `Get-Process | Where-Object { $_.Name -eq "chrome" }` diye anında çekebilirsiniz (Nesne Gümbürtüsü).

**Örnek İşleyiş (Sembolik Olarak):**
Örneğin Çalışan tüm uygulamaları Listelediniz (`Get-Process`). Liste aşağı akarken Linux bunu saf Metin yazısı sanır. PowerShell'in bunu obje olarak tutması şuna izin verir: "İçinde Chrome geçen nesnenin `.Kill()` metodunu tetikle". Yazılımı komut ekranına gömmüştür.

### Örnek Bir PowerShell Kodu: .NET Gücü Olan Konsol
Bir Windows Bilgisayarda çok RAM yiyen (100MB Üstü) veya şüpheli uygulamaları filtreleyip Kapatan (Fiil-isim kurallı) otomasyon gücü:

```powershell
# PowerShell dilinde de yorumlar Bash/Python gibi '#' ile başlar.
# Değişkenler '$' işareti ile oluşturulur. Parametreler/Objeler şemsiye gibi kalkanlanır.

# Kural 1: Ekrana yazi basarken bile 'Ne yapilacak', 'Nereye yazilacak' mantigi (Fiil-İsim) işler.
Write-Host "--- AĞIR İŞLEMLERİ (RAM CANAVARLARINI) TESPİT PANELİ ---" -ForegroundColor Cyan

# Kural 2: İlgili Değişkeni (Eşik Değeri) Oluşturalım (Nesnel olarak 100 MB matematigini bile anliyor)
$RamLimiti = 100MB 

# === OTOMASYON GÜCÜ (PIPELINE '|' MANTIGI VE NESNELER) ===

# ADIM 1: Get-Process (Bütün acık Görev Yöneticisi nesnelerini çek)
# ADIM 2: '|' Borudan(Pipe) Aşağıya aktar ->
# ADIM 3: 'Where-Object' (Nesneyi süz/filtrele), $_ (Borudan akan o anki satir demek), 
#         Eger WorkingSet(KullanilanRam) '-gt'(Greater Than/Büyükse) RamLimiti 
# ADIM 4: '|' Yine boruya filtrelenmişleri aktar ->
# ADIM 5: 'Sort-Object' (Onu siraya Koy), Neye gore(KullanilanRAM'e gore), Azalarak(Descending)

$AgirIslemler = Get-Process | Where-Object { $_.WorkingSet -gt $RamLimiti } | Sort-Object -Property WorkingSet -Descending

# ŞİMDİ KONSOLA RAPOR BASALIM:
Write-Host "Listelenen Programlar (100 MB Üzeri):" -ForegroundColor Yellow

# Foreach (Her biri için dön) (Tıpkı C# döngüsü estetiği)
foreach ($islem in $AgirIslemler) 
{
    # Objenin(Class) içindeki 'Name' (Isim) ve 'WorkingSet'(Ram miktari) property'sine NOKTA ile C# gibi sızdık:
    
    # Rakamlari okunabilir MB (Megabyte) birimine Matematiksel böl (Bilgisayar .NET sayesinde Math eziyeti cekmez)
    $ram_hesabi = [math]::Round($islem.WorkingSet / 1MB, 2)
    
    # Ekrana Şık çıktı ver
    Write-Output "Uygulama: $($islem.Name) | Tuketim: $ram_hesabi MB"
    
    # EGER Chrome ismini istersen, o obje uzerinden ".Kill()" komutu atarak Programi cokertebilirdik!
    # Eger isterseniz su kodu acabilirsiniz: if ($islem.Name -eq "chrome") { Stop-Process -Id $islem.Id -Force }
}

Write-Host "Raporlama Otomatik Olarak Bitti." -ForegroundColor Green
```

Koca Windows Core sistemi bu komut ağaçlarının (Cmdlets) .NET mimarisinden aldığı nesne (Objects) mirasına hizmetkardır. Sisteme istediği rengi verir.

## Kimler Kullanır?
* Milyarlarca dolarlık Şirket Ağlarındaki Kurumsal IT Yöneticileri, Windows Sistem ve Active Directory Administrator'ları (Microsoft ekosisteminde).
* Son yıllarda Hacker'lar (Siber Güvenlik/Pentester). Güvenlik duvarları PowerShello'i "Resmi İşletim sistemi kalbi" saydığı için, Hackerlar Antivirüslere takılmadan RAM içine sızabilmek amacıyla zararlı (.exe) dosyalar üretmek yerine "Dosyasız Zararlılar (Fileless Malware)" diye tabir edilen sızmaları dümdüz uçan PowerShell scriptleri (`Invoke-WebRequest`) üzerinden yürütürler.
