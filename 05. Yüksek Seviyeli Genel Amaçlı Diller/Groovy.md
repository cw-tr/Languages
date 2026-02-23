# Groovy

## Özet
Groovy (Apache Groovy); 2003 yılında James Strachan tarafından icat edilen, Java Sanal Makinesi (JVM) üzerinde çalışan, Java'nın o bitmek bilmeyen uzun ve resmi (boilerplate) kod kalıplarını kırmak için Python ve Ruby dillerinin "Esnekliğini (Dynamic Typing) ve Kısalığını" alıp Java'nın içine zerk eden **JVM tabanlı devasa bir Betik (Scripting) ve Otomasyon** dilidir.

## Nedir ve Ne İşe Yarar?
2000'lerde kurumsal şirketler çok güvende oldukları için Java ile proje yazıyordu. Lakin Java o kadar kuralcı, tip bağımlı (Strict) ve uzundu ki; basit bir "Merhaba" yazdırmak için bile Class'lar ve metodlar döşemeniz zorunluydu. Şirketler, ufak tefek ufak betikler/test dosyaları denemek istediklerinde "Bu Java ile haftalar süren bir eziyet" diyorlardı.

Groovy, Python'un esnekliği ve Ruby'nin Closure (Blok/Fonksiyonel kod aktarımı) zarafetini alıp **direkt Java'ya monte etti**. Bir dosyanın adını `.java` yerine `.groovy` yaparsınız. Eğer isterseniz içine 1'e 1 eski uzun Java kodunuzu yapıştırırsınız (Çünkü tam uyumludur!), İsterseniz noktalı virgülleri, System.out.println'leri, Class isimlerini söküp atarsınız ve Groovy onu tek satırlık bir Script gibi şak diye çalıştırır! Java ile kardeş değil, *Java'nın Mutasyona Uğramış Klonu* gibidir.

**Ne İşe Yarar?**
* **CI/CD Otomasyonları (Jenkins):** Evrendeki devasa Bulut Sunucu/Derleme merkezlerinde saniyede milyonlarca test koşturan **Jenkins Pipeline** dosyalarının kalbi ve tek dili Apache Groovy'dir. (Kurumsal dünyanın Linux Bash'i gibidir).
* **Gradle Build Aracı:** Android uygulamalarının (Kotlin/Java) nasıl derleneceğini söyleyen Gradle dosyaları (Makine emirleri), Groovy (veya Kotlin DSL) kullanılarak kurgulanır. 
* Otomatik Web Testleri (Selenium ve Katalon Studio vb) tamamen Java kütüphanelerini sömürdüğü için Groovy'nin kısalığıyla koşturulur.

## Dilin Mantığı ve Kod Yapısı
Hem Dinamik (Tipi çalışma zamanı belli olan - Python gibi), hem Statik (Tipi derlenirken kontrol edilen - Java gibi) harmanlanmış ucube bir zekadır. İskambil kağıdı gibi kodu isterseniz strict dizersiniz, isterseniz esnetirsiniz.

Java'da mecbur olduğunuz şeyler: Noktalı virgül (`;`), `public/private` belirteçleri, Getter/Setter (Get/Set metodları), ve Print cümlelerinin iğrenç uzunluğu... Groovy'de **hepsi uçurulmuştur**. `println "Merhaba"` yazarsanız Groovy JVM'i arka planda saniyesinde bunu Java Class yapısına çevirip çalıştırır.

**Örnek İşleyiş (Sembolik Olarak):**
Java'daki Döngü: `for(int i=0; i<3; i++) { System.out.println("Java Zordur"); }`
Groovy (Ruby mantığı olan Closure'lar): `3.times { println "Groovy Eğlencelidir!" }`

### Örnek Bir Groovy Kodu: Java'nın O Boğaz Sıkan Formatını Yırtıp Atmak
Bir dosyayı okumak veya basit bir Obje (DTO) üretmek istediğinizde, C#/Java'nın o sayfalarca Set/Get ameleliği yapmasını yırtıp atan mükemmel sentaks:

```groovy
// Groovy Yorumlari C/Java gibi // ile yapilir
// Not: Ayni dosyaya 1'r 1 Java kodu yazsaniz da Groovy derler. Bu JVM'in ozgurlugudur!

/* 1. MUCİZE MÜLAKAT: SINIF (CLASS) VE OTOMATİK GETTER/SETTER */
// "Java'da bu sadece sınıf icin 30 satır Getter/Setter kodu isterdi!"
class Asker {
    String isim  // Public, private yazmaya gerek yok, tip deklare etmek eziyet degil.
    int rutbe 
}


/* 2. ANA METOT (MAIN) ZORUNLULUĞUNU ÇÖPE ATMA */
// Script mantigi. En uca direk kodu basla.
println "--- GROOVY ORDU BİLGİ SİSTEMİ ---"


/* 3. DINAMIK TİPLEME (DEF) VE HARİKA LİSTELER (CLOSURE'LAR) */
// 'def' (Definition) kelimesini tum degiskenlerde JavaScript(Var/let) gibi kullanabilirsiniz:
def ilkAsker = new Asker(isim: "Murat", rutbe: 10) // Otomatik Yapici (Constructor) !
def ikinciAsker = new Asker(isim: "Hakan", rutbe: 5)

// Listeleri Java'daki gibi Array kütüphanesi sömürere değil Köşeli(JSON gibi) bas:
def orduListesi = [ilkAsker, ikinciAsker]

// MUCİZE FONKSİYONEL DÖNGÜ (Ruby Klonu - Closures)
// '.each' metoduyle listenin icinde dön ve Kapanis Blogu(Süslü icindeki 'it') a basla:
orduListesi.each { it ->  
    
    // String İçi Değişken Enjeksiyonu ($ isareti - Java bunu 15 yil sonra zor yapabildi)
    println "Asker Adi: ${it.isim}, Rütbe Gucu: ${it.rutbe}"
}


/* 4. DOSYA OKUMA EZİYETİNİ BİTİRMEK */
// Java'da FileInputStream'lerle Try/Catch yapmadan, Groovy'de Düz okuma:
def dosyaYolu = new File("GizliBelge.txt")

// Eger dosya varsa icindeki satirlari dondur, yoksa kendi Try/Catch arka yapisini cagir (Safe Navigation '?'):
println "Dosya Boyutu: JVM Otomasyonuyla Bulundu -> " + dosyaYolu?.length()

println "Sistem Cikisi: Basarili Coder Harekati."
```
Kod, Python'a çok benzer ama tamamen Amerikan Kurumsal Bankalarının arka odalarındaki Java Sanal Makinesi (JVM - Bytecode) çekirdeklerini çalıştırır. Zengin Java kütüphanelerinin tümüne (`import java.util.*`) tek tıkla ulaşır.

## Kimler Kullanır?
* Yazılım kodlayan veya Web/Mobil tasarım yapan Front-End geliştiriciler nadiren kullanır (Grails Framework'ü hariç).
* ASIL UZMANLARI: **DevOps ve CI/CD Mimarlarıdır**. Eğer dünyada AWS, Azure, Google Cloud gibi ortamlar üzerinde `Jenkins` (Entegre Dağıtım Borusu) sunucusu ile günde 50 defa projenin testlerden otomatik geçip Canlı(Prod) sürüme "Deploy" edildiği bir boru hattı (Pipeline) varsa, o dosyalar (`Jenkinsfile`) %100 Groovy mantığı ile komutlandırılır. Otomasyoncuların ve derleme (Build) mühendislerinin bir numaralı Java-Silahıdır.
