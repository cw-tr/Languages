# Pascal

## Özet
Pascal; 1970 yılında İsviçreli bilgisayar bilimcisi Niklaus Wirth tarafından, öğrencilere "Strukturize (Yapısal), Düzgün ve Sıkı Tipli" programlama kodlamasını mükemmel bir disiplinle **eğitmek amacıyla** yaratılmış; lakin sadece bir eğitim projesi olarak kalmayıp 80'ler ve 90'larda evrendeki ticari Masaüstü Yazılımlarının baş tacı olmuş (özellikle Delphi/Turbo Pascal formuyla) pürüzsüz ve kurallı anıtsal dildir.

## Nedir ve Ne İşe Yarar?
1970'lere gelindiğinde dünya ALGOL'un `begin...end` kuralını öğrenmişti, lakin piyasada var olan C dili adeta Vahşi Batı idi (Hafıza açıklarına, tip kaymalarına izin veriyordu) ve Basic/Fortran dilleri hala Spagetti(GOTO) ile yazılıyordu. 

Niklaus Wirth; "Bilgisayar Mühendisliği öğrencisi kodu rastgele yazmamalıdır! Önce bütün değişkenlerini ve tiplerini sayfanın en başında (VAR bloğunda) deklare etmeye MİNNETTAR olmalıdır. Hata yapmasını dilin yapısı reddetmelidir" diyerek Pascal'ı icat etti. Pascal o kadar temiz, güvenli ve anlaşılırdı ki; üniversite öğrencileri için tasarlanmış olmasına rağmen Şirketler (Apple) ve Bilim İnsanları tarafından kapışıldı. 

**Ne İşe Yarar?**
* **Evrensel Yazılım Eğitimi:** 1970'den 2000'lerin başlarına kadar dünyanın (ve Türkiye'nin) Neredeyse Tümü Bilgisayar Üniversite Algoritmalarını Pascal (ve Turbo Pascal C /Borland) ile öğrenirdi.
* **Apple Lisa ve Erken MacOS:** İlk orijinal Macintosh bilgisayarları ve Lisa işletim sistemi, baştan aşağı Pascal diliyle (Object Pascal) yazılmıştır. (Daha sonra Apple C ve Objective-C'ye geçti).
* **Masaüstü Veritabanı ve Arayüzü (Delphi):** Pascal'ın sadece nesne-yönelim motoru takılmış torunu olan **Delphi (Object Pascal)**, 1990'larda Windows masaüstünde "Sürükle-Bırak butonlu Web/DB formları (RAD - Rapid Application Development)" yapımının en çılgın kralıydı; Visual Basic'in ve C#'ın asıl atasıydı.

## Dilin Mantığı ve Kod Yapısı
Çok ama çok **Kuralcıdır (Strict-Typing / Strict-Structured)**. Bir fonksiyonun/programın neresinde değişken yaratacağınızı siz seçemezsiniz; sayfanın EN ÜSTÜNDEKİ `var` bloğuna yazmak namus borcudur, aşağılarda yaratamazsınız. 

Atama yapma (Assignment) sembolü muazzam ikoniktir ve matematikten çırpmadır: `:=` (Örn: `A := 5;`). Eşittir (`=`) işareti sadece ve sadece İf-Else döngülerinde Kıyaslama (Comparison) (Matematikteki Eşit midir?) işlemi için tahsis edilmiştir. Bloklar C'deki gibi `{}` değil, ALGOL'daki gibi Pür `begin` ve `end.` kelimeleriyle yaratılır.

**Örnek İşleyiş (Sembolik Olarak):**
C dili: `int x = 5; if (x==5) { printf("x besten kucuk"); }`
Pascal dili: `var x: integer; begin x := 5; if x = 5 then writeln('x besten kucuk'); end.`

### Örnek Bir Pascal Kodu: Jilet Gibi Düzenli "Yapısal (Structured) Programlama"
Üniversite hocalarının C'deki spagettilerden yaka silkip, öğrencilere "Bir kod blok blok ve değişken değişken bildirilmelidir!" feryatıyla öğrettikleri klasik ve katı PASCAL hiyerarşisi:

```pascal
{ Pascal'da Cok satırli Yorumlar suslu parantez veya (*  *) isaretleriyle yazilir }

{ Program Adi beyani zorunludur }
PROGRAM HesaplamaTesti;

{ PASCALIN KALBİ (VAR BLOGU): 
  Programda kullanilan tum degiskenler 'Yukaridan asagi' deklare edilmek zorundadır.
  Programin icinde rastgele "int a = 5" VEYA "x := 10" yaratamazsiniz, 
  Dil derlemeyi anidan kilitler ve Tokadi basar! }
VAR
    kullaniciNotu: Integer;
    isim: String;
    gecerMi: Boolean;

{ PROSEDÜRLER (Void Func) Veya FOKSYONLAR }
PROCEDURE NotuKontrolEt(notDegeri: Integer);
BEGIN
    { Atama Mimarisi ( := ) ve Kıyaslama Mimarisi ( = veya >, < ) }
    IF notDegeri >= 50 THEN
        BEGIN
            gecerMi := True;
            Writeln('Tebrikler, Dersi basariyla Gectiniz!');
        END
    ELSE
        BEGIN
            gecerMi := False;
            Writeln('Malesef kaldiniz. Gelecek seneye bekleriz.');
        END;
END;


{ === ASIL PROGRAM BLOGU === }
BEGIN
    isim := 'Murat';
    Writeln('Ogrenci Adi: ', isim);

    Write('Lutfen Sinav Notunuzu Girin: ');
    Readln(kullaniciNotu); { Console'dan C dili scanf'i eziyeti olmadan Degiskene al! }
    
    { Yukaridaki Proseduru Cagirma }
    NotuKontrolEt(kullaniciNotu);
    
END. { Ana program MUTLAKA 'END.' (Noktali End) ile biter, odaciklar noktalı virgül(;)  }
```

Bu dörtdörtlük hiyerarşi, bir programlama dili hatasının C'deki gibi hafıza Pointer (İşaretçi) sızıntısına veya yanlış satıra kaymasına engel olan kusursuz bir kılıftı.

## Kimler Kullanır?
* Orijinal hali (Standard Pascal) ölmüş olsa da, Anders Hejlsberg (C# ve TypeScript'in yaratıcısı ve yaşayan efsane) tarafından modifiye edilen **Delphi (Object Pascal)** versiyonu hala, hala ve hala Eski (Legacy) Kurumsal Windows Kamu Daireleri yazılımlarında (Hastaneler, Belediyeler, Otomasyon cihazları) yaşamaktadır.
* Lazarus ve Free Pascal adı altında açık kaynak dünyasında çapraz-platform Masaüstü Form Cihazları yazan Sadık Mühendisler ve retro geliştiriciler tarafından kullanılır. 
* Yeni ticari projelerde yerini baştan ayağa C#, Java ve Typescript'e devretmiştir.
