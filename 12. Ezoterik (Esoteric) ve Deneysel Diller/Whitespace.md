# Whitespace

## Özet
Whitespace (Boşluk); 2003 yılında Edwin Brady ve Chris Morris (İngiltere'deki Durham Üniversitesi araştırmacıları) tarafından tamamen 1 Nisan Şakası (April Fools' Day) olarak yayınlanan; Koddaki tüm görünür harflerin, rakamların ve sembollerin Yok Sayıldığı, Derleyicinin (Compiler) sadece ve sadece **Boşluk (Space), Tab ve Enter (Line Feed)** tuşlarını algoritma olarak okuyup programladığı, evrendeki "Görünmez Koda Sahip" tek ezoterik dildir.

## Nedir ve Ne İşe Yarar?
Tarih boyunca bütün yazılım dilleri görünür ASCII karakterler üstüne (a-z, = , ;, {}) kurulmuştur. Bilgisayar bu dilleri derlerken "Space (Boşluk)" tuşlarını genelde okunabilirlik (Indentation) saydığı için çöpe atar ve görmezden gelir (Ignored). 

Whitespace yaratıcıları bu isyanın intikamını aldı: "Boşlukları ezen kodlara karşı biz öyle bir dil yapalım ki, alfabedeki her şey Yorum satırı (Çöp) sayılsın; SADECE Space, Tab ve Return tuşu Matematiksel şifreye dönüşsün!". Ortaya inanılmaz "gizli ve hayalet" gibi bir konsept çıktı.

**Ne İşe Yarar?**
* **Steganografi (Gizli Mesaj Saklama):** Çok karanlık ama müthiş bir numarası vardır. Siz normal, patronunuzun okuması için C++ ile bir kod yazarsınız. O kodun satır sonlarına "Gereksiz boşluklar ve Tablar" atarsınız. Patronunuz kodu çalıştırır ve e-ticaret siteniz düzgün açılır. Lakin siz O C++ kodunu "Whitespace Derleyicisine" Sokarsanız, Aralara gizlenmiş olan boşluklar uyanır ve Patronun Bilgisayarındaki dosyaları silen Bir Virüse (Keylogger) dönüşür! 
* Evrendeki en görünmez Çoklu-Dil (Polyglot) yeteneğidir (İki dili aynı metin dosyasında çaktırmadan harmanlamak).

## Dilin Mantığı ve Kod Yapısı
Sistem çok basittir, bir tür Stack (Yığın) tabanlı Makine Algoritmasıdır. 3 Tuştan oluşan bir Mors Alfabesidir:
- **`[Space] (Boşluk)`**
- **`[Tab] (Sekme)`**
- **`[LF] (Satır Sonu / Enter)`** 

Bir komut her zaman "Neyi yapacağı (Instruction Modification Parent)" parametresiyle başlar:
Örneğin Stack'e sayı mı ekleyeceksin? Kodu: `[Space]`
Aritmetik (Toplama filan) mi yapacaksın? Kodu: `[Tab][Space]`

Arkasına Parametre alıyorsa (sayılar); Sayıların İkili Mantığı şöyledir: `Space` 0 demektir, `Tab` 1 Demektir.

**Örnek İşleyiş (Sembolik Olarak):**
Yığına(Bellek) "1" sayısını atmak: (1.Emir: Space) -> (2.Sayı İsareti: Artı ise Space) -> (3.Rakamın İkilik Hali: Tab) -> (4.Komut Bitti: LF).
Yani Toplamda "Boşluk, Boşluk, TAB, Enter" aklına basarsanız sisteme Gizli bir 1 rakamı atamış olursunuz.

### Örnek Bir Whitespace Kodu: C++ Kodunun İçine "Hello World" Virüsü Gömmek
Aşağıdaki kodda Gözüken Harflerin (Print, int vs) HIÇBIR ANLAMI YOKTUR, Onlar Yorum Çöpüdür. Asıl çalışan şeyler, o harflerin sonundaki (Seçerek ancak bilebileceğiniz) görünmez Boşluklardır.
*(Temsili Gösterim İçin: (S)pace , (T)ab, (L)ineFeed olarak yazılmıştır lakin aslı şeffaftır!)*

```whitespace
// BU BIR WHITESPACE KODUDUR (Harfler kandirmacadir)

int main() {         S S S T S S T S S S L    // H!
    return 0;        T L S S S S S T T S S T S T L  // e!
}                    T L S S S S S T T S T T S S L  // l!
                     T L S S S S S T T S T T O S L  // l!
                     T L S S S S S T T S T T T T L  // o!
```

Görsel Text editöründe kodu açtığınızda bomboş beyaz bir ekran vardır. Lakin dosyayı imleçle (Fareyle Shift ile Drag ederek) Seçtiğiniz zaman "Bazı satırların mavi highlight/Pürüzlerinin kısa veya uzun" olduğunu fark edersiniz. Derleyici klasördeki görünmez Tabları hesaplar ve "Hello World" matematiğini ekranda patlatır.

## Kimler Kullanır?
* 1 Nisan şakası/Mizahşörler ve Kripto/Şifreleme heveslileri. 
* Kötü emelli Hack/Virüs gösterilerinde; Kod incelemesi (Code Review) yapan yöneticiler veya Anti-Virus firmaları "Gözle Görünür" Zararlı bir komut (Örn System32.delete) bulamazsa, Yazılımcı kötücül kodunu Dosyanın içine sadece Boşluk/Space Tuşlarıyla örebilir! Bu bağlamda bir Şakadan ibaret olsa da "Şeffaf Yazılım" mantığının yegane vücut bulmuş fütüristik halidir.
