# Delphi (Object Pascal)

## Özet
Delphi (Asıl Dili: Object Pascal); 1995 yılında Borland şirketi (baş mimarı Anders Hejlsberg) tarafından geliştirilen, eski/katı Pascal diline Nesne Yönelimli Programlama (OOP) ve muazzam bir Grafiksel Bileşen Mimarisi (VCL) ekleyerek, Windows masaüstü uygulamalarını "Sürükle-Bırak" (RAD) mantığıyla saniyeler içinde yaratan 90'lı yılların "Efsanevi" sistem dilidir.

## Nedir ve Ne İşe Yarar?
1990'ların ortalarında Windows işletim sistemine (Windows 95) grafiksel bir program yazmak C++ ile aylar süren bir işkenceydi. Pointerlar, hafıza açıkları, ekrana buton çizmek için yüzlerce satır API (Win32) kodu dökmek gerekiyordu. Visual Basic vardı ama o da hız olarak felaket derecede yavaştı (Interpreted).

İşte Delphi, dünyadaki C++ hızını "Görsel Geliştirme (Visual Basic) rahatlığıyla" birleştiren bir cennet oldu. Ekrana bir Button sürükleyip bırakıyordunuz, üzerine çift tıklayıp `Object Pascal` dilinde arkasına `ShowMessage('Merhaba');` yazıyordunuz ve program saliseler içinde Native (Saf EXE makine koduna) derleniyordu. Hiçbir DLL kütüphanesine veya JVM sanal makinesine ihtiyaç duymadan, saf hızda, saniyeler içinde program üretiyordu. (RAD - Hızlı Uygulama Geliştirme krallığı).

**Ne İşe Yarar?**
* **Windows Masaüstü (UI) Mimarisi:** Muhasebe programları, Hastane Otomasyonları, Stok-Kasa barkod yazılımları, Kütüphane takip sistemleri. Türkiye'deki ve dünyadaki küçük/orta işletme (KOBİ) masaüstü yazılım tarihinin %80'i Delphi (ve onun kütüphanesi VCL - Visual Component Library) ile inşa edilmiştir.
* **Veritabanı (BDE/ADO) Krallığı:** Delphi; SQL, DBase, Paradox veya Oracle gibi uzak/yerel veritabanlarına sadece 2 bileşen tıklatarak muazzam bir şık tablo (DBGrid) ile anında (0 kod ile bile) veri bağlayabilirdi.

## Dilin Mantığı ve Kod Yapısı
Temel dil **Object Pascal**'dır. Yani standard Pascal'ın (`var`, `begin...end`, `:=` atamaları) o katı bloklarına; modern yazılımın anahtarları olan Classes (Sınıflar), Properties (Özellikler) ve Exceptions (Hata Ayıklama Try/Except) zırhları giydirilmiştir.

Nesneler, C++ gibi arka planda görünmez şekilde Pointer (Referans) bazlıdır ancak programcı yıldız `*` yazarak pointer kavgası vermez. Form'un (Pencerenin) kendisi bir CLASS'tır ve sizin sürüklediğiniz her Buton, Text kutusu o Form Sınıfının bir "Üyesidir". 

**Örnek İşleyiş (Sembolik Olarak):**
Buton1 (TButton) nesnesine tıklandığında ekranın kapanması için Object Pascal; `Form1` nesnesini atıflar:
`procedure TForm1.Button1Click(Sender: TObject); begin Close; end;`

### Örnek Bir Delphi Kodu: Görsel Olay Yönelimli İşletim ve Nesneler
Bir Formun içine açılan ve Formun elemanlarını kontrol eden katı (Strict-Typed) ve çok anlamsal (İngilizce Cümle Yapılı) Object Pascal kurgusu:

```pascal
{ Yorum satırları süslü parantez içine hapsedilmiştir. }

{ Arayuz (Interface) Bloğu: O sayfada kullanilacak Kutuphaneler (Uses) ve Sınıf iskeleti. 
  IDE (Embarcadero/Borland) tarafindan siz sürükle bırak yaptıkça otomatik uretilir. }
unit AnaPencere;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

{ Pencere (Form1) bir Siniftir (TForm kokeninden Miras/Inheritance almistir) }
type
  TForm1 = class(TForm)
    HosgeldinButonu: TButton;         { Bizim ekrana Şak(?) diye koydugumuz buton }
    SonucEtiketi: TLabel;             { Yazi Kutusu }
    
    { Butona cift tiklayinca otomatik olusan Olay (Event/Mesaj) tetikleyici Proseduru }
    procedure HosgeldinButonuClick(Sender: TObject);
  private
    { Özel Degiskenler (Class Field) }
    MusteriAdi: String;
  public
    { Acik Prosedurler }
  end;

var
  Form1: TForm1;

{ ======================================= }
{ BURASI KODUN UYGULAMAYA GECTIGI ASIL BÖLÜM (IMPLEMENTATION) }
implementation

{$R *.dfm} { Bu .dfm Formun gorsel (Butonuz x ve y koordinatı vb) cizim dosyasini arkaplanda baglar }

{ Prosedürün Govdesi (Form1 sınıfına ait oldugu TForm1. seklinde Sahiplenilmis) }
procedure TForm1.HosgeldinButonuClick(Sender: TObject);
var
   { KURAL: Degiskenler (Vars) sadece BEGIN kulesinden ONCE deklare EDİLİR! }
   hesaplama: Integer;
begin
    
    { Sınıfın (MusteriAdi) niteligine, := atamasıyla deger yerleştiriyoruz }
    MusteriAdi := 'Sayın Siber CEO';
    
    hesaplama := 10 * 5;

    { Delphi Kütüphanesinin (VCL) O muazzam ozellikleri: 
      Sonuc etiketinin 'Yazi (Caption)' Objesine sız ve Ekrani saniyesinde yenile! 
      (IntToStr -> Sayisi metne cevirir, aksi takdirde Pascal anında hata basar ve derlemez) }
      
    SonucEtiketi.Caption := 'Hosgeldiniz ' + MusteriAdi + '! Sonuc = ' + IntToStr(hesaplama);
    
    { Ekrana Pop-Up MessageBox Firlat! (Win32 Çağrısıdır ama Delphi Çok Basite İndirger) }
    ShowMessage('Sistem Basariyla Guncellendi!');

end;

end. { Nokta ile Kapanis, o Unit(Dosyanin) bittiginin Muhurudur. }
```
Siz "Run / F9" tuşuna bastığınızda bu devasa Object Pascal yığını tek bir EXE dosyasına sıkıştırılır (Sadece 2-3 Megabyte eder). Kurulum gerekmeden FlashDisk ile bir Windows'a atıp takır takır çalıştırırsınız.

## Kimler Kullanır?
* Evrendeki bütün KOBİ, Muhasebe (Türkiye'de Logo, Mikro, Akınsoft vb. yazılımlarının çoğu), Stok ve Restoran Adisyon uygulamalarının "Eski Toprak" 90'lar/2000'ler nesli geliştiricileri.
* Günümüzde **Embarcadero Delphi** ismiyle dev bir ekosistem olarak satılmaya devam eder ancak eski şöhreti kalmamıştır (Çünkü C#/.NET ve Web (Javascript) dünyası masaüstü sektörünü paramparça etmiştir).
* Delphi'nin yaratıcısı Anders Hejlsberg şirket batmaya doğru gidince Microsoft'a transfer olmuş, ve C++ dilini "Delphi Görselliği ve RAD mantığıyla" birleştirip bildiğiniz **C#** (ve visual studio'yu) icat ederek dünyayı ele geçirmiştir.
